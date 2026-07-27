#options(shiny.launch.browser = TRUE)
options(sass.cache = tempfile())

compartment_labels <- LeggettPlus:::LEGGETT_COMPARTMENT_LABELS

compartment_groups <- list(
  Plasma = c("plasd", "plasb"),
  Blood = c("rbc"),
  `Extracellular Fluid` = c("evf"),
  Bone = c("csf", "cex", "cne", "tsf", "tex", "tne"),
  Liver = c("livhu", "livlu"),
  `Soft Tissue` = c("strpd", "stmid", "stslo"),
  Brain = c("brain"),
  `Kidney & Urinary` = c("kidoth", "kidurp"),
  Intestines = c("sint", "lintu", "lintl")
)

compartment_to_group <- unlist(lapply(
  names(compartment_groups),
  function(group) {
    compartments <- compartment_groups[[group]]
    stats::setNames(rep(group, length(compartments)), compartments)
  }
))

# ---- OEHHA breathing rates tables (excerpted) ----
OEHHA_BR_RES_DAILY_LKGD <- tibble::tribble(
  ~age_band       , ~mean , ~p95 ,
  "3rd trimester" ,   225 ,  361 ,
  "0-<2 y"        ,   658 , 1090 ,
  "2-<9 y"        ,   535 ,  861 ,
  "2-<16 y"       ,   452 ,  745 ,
  "16-<30 y"      ,   210 ,  335 ,
  "16-70 y"       ,   185 ,  290
)

OEHHA_BR_WORK_8H_LKG8H <- tibble::tribble(
  ~intensity                 , ~mean , ~p95 ,
  "Sedentary/passive (<1.5)" ,    30 ,   40 ,
  "Light (1.5–<3.0)"       ,    80 ,  100 ,
  "Moderate (3.0–<6.0)"    ,   170 ,  230
)

# ---- Job → activity presets (maps Table 5.9 categories to Table 5.8 intensities) ----
JOB_PRESETS <- tibble::tribble(
  ~label                        , ~intensity              , ~percentile   ,
  "Custom (no preset)"          , NA_character_           , NA_character_ ,
  "Administrative office work"  , "Light (1.5–<3.0)"    , "p95"         ,
  "Sales / Professional"        , "Light (1.5–<3.0)"    , "p95"         ,
  "Technicians / Production"    , "Moderate (3.0–<6.0)" , "p95"         ,
  "Machinists"                  , "Moderate (3.0–<6.0)" , "p95"         ,
  "Service / Private household" , "Moderate (3.0–<6.0)" , "p95"         ,
  "Farming"                     , "Moderate (3.0–<6.0)" , "p95"
)


# Pregnant workers: use 16–<30 y moderate row per OEHHA note for 3rd trimester

# ---- Unit converters ----
Lkg8h_to_m3_per_h <- function(Lkg8h, bw_kg) {
  # convert (L/kg per 8h) * kg / 8 h = L/h; then /1000 = m3/h
  (Lkg8h * bw_kg / 8) / 1000
}
Lkgd_to_m3_per_h <- function(Lkgd, bw_kg) {
  # (L/kg-day) * kg / 24 h = L/h; then /1000 = m3/h
  (Lkgd * bw_kg / 24) / 1000
}

# Parse OEHHA age-band labels into numeric ranges
# Returns list(min, max, upper_inclusive)
parse_age_band <- function(label) {
  if (is.null(label) || is.na(label)) {
    return(list(min = NA_real_, max = NA_real_, upper_inclusive = FALSE))
  }
  lab <- trimws(tolower(label))
  # Normalize variants
  lab <- gsub("years|year|y", "", lab)
  lab <- gsub("\\s+", "", lab)

  # Special case: 3rd trimester (maternal, not an age bin)
  if (grepl("^3rdtrimester", lab)) {
    return(list(min = NA_real_, max = NA_real_, upper_inclusive = FALSE))
  }

  # Patterns like "0-<2", "2-<9", "16-<30"
  m_lt <- regexpr("^(\\d+)-<(\\d+)$", lab, perl = TRUE)
  if (m_lt[1] != -1) {
    lo <- as.numeric(sub("^(\\d+)-<(\\d+)$", "\\1", lab))
    hi <- as.numeric(sub("^(\\d+)-<(\\d+)$", "\\2", lab))
    return(list(min = lo, max = hi, upper_inclusive = FALSE))
  }

  # Pattern like "16-70"
  m_dash <- regexpr("^(\\d+)-(\\d+)$", lab, perl = TRUE)
  if (m_dash[1] != -1) {
    lo <- as.numeric(sub("^(\\d+)-(\\d+)$", "\\1", lab))
    hi <- as.numeric(sub("^(\\d+)-(\\d+)$", "\\2", lab))
    return(list(min = lo, max = hi, upper_inclusive = TRUE))
  }

  # Fallback
  list(min = NA_real_, max = NA_real_, upper_inclusive = FALSE)
}

# Check if [start_age, end_age] is fully inside the band
age_span_within_band <- function(start_age, end_age, band) {
  if (is.na(band$min) || is.na(band$max)) {
    return(NA)
  } # not applicable (e.g., 3rd trimester)
  if (!is.finite(start_age) || !is.finite(end_age)) {
    return(NA)
  }
  lo_ok <- start_age >= band$min
  hi_ok <- if (isTRUE(band$upper_inclusive)) {
    end_age <= band$max
  } else {
    end_age < band$max
  }
  lo_ok && hi_ok
}


# ---- WAF/DF helpers (Eq 5.4.1.2 B–C) ----
compute_WAF <- function(Hsource, Dsource, DF = 1) {
  # Hres = 24; Dres = 7
  (24 / Hsource) * (7 / Dsource) * DF
}
compute_DF <- function(Hcoincident, Hworker, Dcoincident, Dworker) {
  (Hcoincident / Hworker) * (Dcoincident / Dworker)
}


add_tooltip <- function(tag, text) {
  if (is.null(text) || text == "") {
    return(tag)
  }
  existing_class <- tag$attribs$class
  new_class <- if (is.null(existing_class)) {
    "has-tooltip"
  } else {
    paste(existing_class, "has-tooltip")
  }
  htmltools::tagAppendAttributes(tag, title = text, class = new_class)
}

generate_group_shades <- function(color, n) {
  # If just 1 compartment, return the base color normalized to hex
  if (n <= 1) {
    rgb <- grDevices::col2rgb(color)
    return(grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255))
  }

  # Convert base color to HSV to grab hue, then to HCL ranges we control
  hsv_vals <- grDevices::rgb2hsv(grDevices::col2rgb(color) / 255)
  h <- as.numeric(hsv_vals["h", 1]) * 360 # degrees for HCL

  # Safe, readable ranges (tunable):
  # - Lightness stays high-to-mid so nothing is near black
  # - Chroma starts moderate and eases down to avoid neon / muddy extremes
  L_hi <- 80
  L_lo <- 55 # never below ~55 (keeps lines visible)
  C_hi <- 60
  C_lo <- 35

  L_seq <- seq(L_hi, L_lo, length.out = n)
  C_seq <- seq(C_hi, C_lo, length.out = n)

  # Small hue jitter when many items to avoid indistinguishable shades
  if (n >= 5) {
    jitter <- seq(-6, 6, length.out = n) # degrees
  } else {
    jitter <- rep(0, n)
  }

  hex <- vapply(
    seq_len(n),
    function(i) {
      grDevices::hcl(
        h = (h + jitter[i]) %% 360,
        c = C_seq[i],
        l = L_seq[i]
      )
    },
    character(1)
  )

  # Uppercase, six-digit hex guarantee; fallback (shouldn’t trigger)
  hex <- toupper(hex)
  hex[!grepl("^#[0-9A-F]{6}$", hex)] <- "#999999"
  hex
}


group_base_colors <- c(
  Plasma = "#1f77b4",
  Blood = "#2ca02c",
  `Extracellular Fluid` = "#9467bd",
  Bone = "#8c564b",
  Liver = "#ff7f0e",
  `Soft Tissue` = "#17becf",
  Brain = "#bcbd22",
  `Kidney & Urinary` = "#e377c2",
  Intestines = "#7f7f7f"
)

compartment_palette <- purrr::imap_dfr(
  compartment_groups,
  function(compartments, group) {
    base_color <- group_base_colors[[group]]
    shades <- generate_group_shades(base_color, length(compartments))
    tibble::tibble(
      compartment_raw = compartments,
      compartment = unname(compartment_labels[compartments]),
      general_group = group,
      color = shades
    )
  }
) |>
  dplyr::mutate(
    general_group = factor(general_group, levels = names(compartment_groups))
  )

compartment_colors <- setNames(
  compartment_palette$color,
  compartment_palette$compartment
)
compartment_label_levels <- compartment_palette$compartment
compartment_group_levels <- levels(compartment_palette$general_group)

scenario_descriptions <- list(
  occupational = paste(
    "Represents consistent daily exposures during working years.",
    "Occupational and non-occupational breathing rates and concentrations",
    "are applied for the entire exposure duration."
  ),
  occupational_retirement = paste(
    "Extends the occupational scenario by including a retirement period",
    "with reduced exposures after working years are complete."
  ),
  pulse = paste(
    "Models a series of discrete exposure events that occur on top of",
    "background conditions. Pulse settings control the size and spacing",
    "of each exposure event."
  )
)

# Little chip style for equations
eq_chip <- function(x) {
  shiny::tags$code(
    style = "padding:.15rem .4rem;border-radius:6px;background:#f7f7f9;border:1px solid #eee;display:inline-block;",
    x
  )
}

ui <- shiny::navbarPage(
  title = shiny::tagList(
    shiny::tags$img(
      src = "leggettplus_wb.jpg",
      alt = "LeggettPlus logo",
      height = "28",
      style = "margin-right: .4rem;"
    ),
    #shiny::icon("dna"),
    "Lead Legget+ Simulator"
  ),
  position = "fixed-top",
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter Tight"),
    primary = "#0B6E99",
    success = "#2ca02c",
    info = "#17becf",
    warning = "#ff7f0e",
    danger = "#d9534f"
  ),
  header = shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      "
    /* --- General UI polish --- */
    .has-tooltip label { cursor: help; }
    .download-controls { display:flex; flex-wrap:wrap; gap:.75rem; margin-bottom:1rem; }
    .about-image-wrapper { text-align:center; margin-top:1.5rem; }
    .about-image-wrapper img { max-width: 720px; width:100%; height:auto; border-radius:12px; box-shadow:0 4px 18px rgba(0,0,0,.12); }
    .card-tight .card-body { padding: .75rem 1rem; }
    .section-title { margin: 0 0 .25rem 0; display:flex; align-items:center; gap:.5rem; }
    .section-title .fa { opacity:.8; }
    .tab-pane[data-value='tab_about'] .section-title { margin-top: 2.25rem; }
    .tab-pane[data-value='tab_about'] p { margin-bottom: 1rem; }
    .tab-pane[data-value='tab_about'] ul { margin-bottom: 1.25rem; }
    .tab-pane[data-value='tab_about'] > .container-fluid > .accordion,
    .tab-pane[data-value='tab_about'] > .container-fluid > pre { margin-bottom: 1.5rem; }
    .sticky-run {
      position: sticky; bottom: 0; z-index: 10;
      background: rgba(255,255,255,.85); backdrop-filter: blur(6px);
      padding: .75rem 1rem; border-top: 1px solid #e9ecef; border-radius: 12px;
    }
    .pill { display:inline-flex; align-items:center; gap:.4rem; border-radius:999px; padding:.2rem .6rem; font-size:.85rem; background:#e8f4fa; color:#0B6E99; }
    .muted { color:#6c757d; }
    .kpi { display:flex; gap:.75rem; align-items:center; }
    .kpi .num { font-weight:700; }
    .subtle { font-size: .92rem; color:#5f6b7a; }
    .hr-soft { border-top:1px dashed #e2e6ea; margin:.75rem 0; }
    .label-unit small { color:#6c757d; font-weight:500; margin-left:.25rem; }

    /* Badges (scenario echo) */
    .badge-wrap { display:flex; align-items:center; gap:.5rem; margin:.25rem 0 1rem 0; }
    .badge { display:inline-flex; align-items:center; gap:.5rem; padding:.25rem .65rem; border-radius:999px; font-weight:600; }
    .badge-occ { background:#e8f5e9; color:#2e7d32; }
    .badge-ret { background:#fff3e0; color:#e65100; }
    .badge-pulse { background:#e3f2fd; color:#0d47a1; }

    /* --- Results tab layout (scoped with #results-layout) --- */
    /* ---- Fix for sidebar overlapping main results panel ---- */
    #results-layout .sidebar {
    flex: 0 0 300px !important;   /* stable preferred width */
    max-width: 30% !important;    /* prevents it from intruding into main panel */
    min-width: 260px;
    z-index: 2;                   /* keeps it underneath, not over, the plot card */
    }
    
    /* Ensure main panel can expand properly */
    #results-layout .main {
    flex: 1 1 auto !important;
    min-width: 0 !important;      /* required to allow shrinking */
    }
    
    /* Sidebar title should not exceed its column */
    #results-layout .sidebar .sidebar-title,
    #results-layout .sidebar .card-header,
    #results-layout .sidebar .accordion-button {
      max-width: 100%;
      box-sizing: border-box;
      /* choose ONE of the two behaviors below: ellipsis (nowrap) OR wrapping */
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }

    /* If you prefer wrapping instead of ellipsis, comment the 3 lines above and use: */
    /* #results-layout .sidebar .sidebar-title,
       #results-layout .sidebar .card-header,
       #results-layout .sidebar .accordion-button { white-space: normal; word-break: break-word; } */

    /* Allow long control labels to wrap nicely */
    #results-layout .sidebar .form-group label,
    #results-layout .sidebar .checkbox label,
    #results-layout .sidebar .radio label {
      white-space: normal;
      word-break: break-word;
      line-height: 1.25;
      margin-bottom: .35rem;
    }

    /* Optional: split long checkbox lists into two columns */
    #results-layout .checkbox-group {
      columns: 2;
      column-gap: 1rem;
    }
    #results-layout .checkbox-group .checkbox {
      break-inside: avoid;
    }

    /* Accordion button alignment (subtle) */
    #results-layout .accordion-button {
      align-items: center;
    }

    /* Responsive tweaks */
    @media (max-width: 1200px) {
      #results-layout .sidebar { flex-basis: 320px; max-width: 360px; }
    }
    @media (max-width: 992px) {
      #results-layout .sidebar { flex-basis: 100%; max-width: 100%; }
      #results-layout .sidebar .sidebar-title,
      #results-layout .sidebar .card-header,
      #results-layout .sidebar .accordion-button {
        white-space: normal; text-overflow: clip; overflow: visible;
      }
    }
    
    /* Bottom-left toast notifications */
    shiny-notification {
    position: fixed !important;
    left: 16px !important;
    bottom: 16px !important;
    right: auto !important;
    top: auto !important;
    max-width: 380px;
    z-index: 3000;
    box-shadow: 0 8px 24px rgba(0,0,0,.18);
    border-radius: 10px;
    }
  "
    ))
  ),

  # -------- Scenario Design --------
  shiny::tabPanel(
    title = "Scenario Design",
    value = "tab_scenario_design",
    icon = shiny::icon("chalkboard-user"),
    shiny::fluidPage(
      # Intro
      shiny::div(
        class = "mb-3",
        shiny::br(),
        shiny::br(),
        shiny::h1(class = "mb-1", "Design Exposure Scenario"),
        shiny::p(
          class = "subtle",
          shiny::icon("info-circle"),
          " The Leggett+ PBPK model simulates Pb exposure via inhalation and other sources,",
          " tracking concentrations across internal compartments with user-defined scenarios."
        )
      ),

      # Row 1: Scenario Type + Description
      bslib::layout_columns(
        col_widths = c(5, 7),
        # Scenario card
        bslib::card(
          class = "card-tight",
          bslib::card_header(shiny::div(
            class = "section-title",
            shiny::icon("sliders"),
            "Scenario Type"
          )),
          add_tooltip(
            shiny::selectInput(
              inputId = "simulation.type",
              label = shiny::tagList(
                shiny::span("Simulation Type"),
                shiny::span(
                  class = "pill",
                  shiny::icon("wand-magic-sparkles"),
                  "template-driven"
                )
              ),
              choices = c(
                "Occupational" = "occupational",
                "Occupational with Retirement" = "occupational_retirement",
                "Pulse" = "pulse"
              ),
              selected = "occupational"
            ),
            "Choose how exposures are structured over time."
          ),
          shiny::div(class = "hr-soft"),
          shiny::uiOutput("scenario_description")
        ),

        # Scenario quick tips / badges
        bslib::card(
          class = "card-tight",
          bslib::card_header(shiny::div(
            class = "section-title",
            shiny::icon("lightbulb"),
            "Quick Tips"
          )),
          shiny::tags$ul(
            class = "subtle",
            shiny::tags$li(
              "Occupational: steady workplace + non-occupational exposures over set years."
            ),
            shiny::tags$li(
              "Retirement: working years followed by reduced/zero workplace exposure."
            ),
            shiny::tags$li(
              "Pulse: inject discrete exposure spikes on top of background (or set background to zero)."
            )
          ),
          shiny::div(class = "hr-soft"),
          shiny::div(
            class = "kpi subtle",
            shiny::span(
              shiny::icon("calendar"),
              shiny::strong("Years of Exposure:"),
              "controlled in Time Parameters"
            ),
            shiny::span("•"),
            shiny::span(
              shiny::icon("lungs"),
              shiny::strong("Breathing Rates:"),
              "toggle with scenario"
            )
          )
        )
      ),

      # Row 2: Three compact cards – Person / Time / Start
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        # Person
        bslib::card(
          class = "card-tight",
          bslib::card_header(shiny::div(
            class = "section-title",
            shiny::icon("user"),
            "Person"
          )),
          # Mode toggle
          add_tooltip(
            shiny::radioButtons(
              "br_mode",
              "Breathing Rate Mode",
              choices = c(
                "Manual (m3/hour)" = "manual",
                "Office of Environmental Health Hazard Assessment-derived " = "oehha"
              ),
              selected = "manual",
              inline = TRUE
            ),
            paste(
              "Manual uses the user-entered rates.",
              "OEHHA mode uses the Office of Environmental Health Hazard Assessment",
              "(OEHHA) Air Toxics Hot Spots Program Guidance Manual (Feb 2015),",
              "Appendix A, Table 5.6 (residential) and Table 5.8 (8-hour worker)",
              "values converted from L/kg-day or L/kg-8h to m3/hour using body weight."
            )
          ),

          # Manual inputs
          shiny::conditionalPanel(
            condition = "input.br_mode == 'manual' && input['simulation.type'] != 'pulse'",
            add_tooltip(
              shiny::numericInput(
                "occ.breath.rate",
                shiny::tagList(
                  "Occupational Breathing Rate",
                  shiny::span(class = "label-unit", shiny::tags$small("(m³/hour)"))
                ),
                value = 1.25,
                min = 0,
                step = .25
              ),
              "Average breathing rate during occupational hours."
            ),
            add_tooltip(
              shiny::numericInput(
                "Nonocc.breath.rate",
                shiny::tagList(
                  "Non-Occupational Breathing Rate",
                  shiny::span(class = "label-unit", shiny::tags$small("(m³/hour)"))
                ),
                value = 0.714,
                min = 0,
                step = .25
              ),
              "Average breathing rate outside of work."
            )
          ),

          # OEHHA-derived inputs
          shiny::conditionalPanel(
            condition = "input.br_mode == 'oehha'",
            # Residential (non-occupational)
            add_tooltip(
              shiny::selectInput(
                "oehha_res_age",
                "Residential Age Band",
                choices = OEHHA_BR_RES_DAILY_LKGD$age_band,
                selected = "16-70 y"
              ),
              "Office of Environmental Health Hazard Assessment (OEHHA) Air Toxics Hot Spots Program Guidance Manual (Feb 2015), Appendix A, Table 5.6 (residential daily breathing rates, L/kg-day)."
            ),
            shiny::radioButtons(
              "oehha_res_pct",
              "Residential Percentile",
              choices = c("Mean" = "mean", "95th" = "p95"),
              selected = "p95",
              inline = TRUE
            ),
            shiny::div(class = "hr-soft"),

            # Worker presets + activity
            add_tooltip(
              shiny::selectInput(
                "job_preset",
                "Job Preset",
                choices = JOB_PRESETS$label,
                selected = "Custom (no preset)"
              ),
              "Maps Office of Environmental Health Hazard Assessment (OEHHA) Table 5.9 job categories to Table 5.8 intensity bands."
            ),
            add_tooltip(
              shiny::selectInput(
                "oehha_work_intensity",
                "Work Activity Intensity (8-hr)",
                choices = OEHHA_BR_WORK_8H_LKG8H$intensity,
                selected = "Moderate (3.0–<6.0)"
              ),
              "Office of Environmental Health Hazard Assessment (OEHHA) Air Toxics Hot Spots Program Guidance Manual (Feb 2015), Appendix A, Table 5.8 (8-hour worker breathing rates, L/kg-8h)."
            ),
            shiny::radioButtons(
              "oehha_work_pct",
              "Work Percentile",
              choices = c("Mean" = "mean", "95th" = "p95"),
              selected = "p95",
              inline = TRUE
            ),

            # Guardrail: pregnancy hint
            shiny::uiOutput("pregnancy_hint"),
            shiny::div(class = "hr-soft"),
            shiny::htmlOutput("oehha_br_echo")
          ),

          add_tooltip(
            shiny::numericInput(
              "body.weight",
              shiny::tagList(
                "Body Weight",
                shiny::span(class = "label-unit", shiny::tags$small("(kg)"))
              ),
              value = 73,
              min = 1,
              step = 0.5
            ),
            "Body weight scales physiological volumes and Office of Environmental Health Hazard Assessment (OEHHA) breathing-rate conversions."
          ),
          shiny::p(
            class = "muted",
            shiny::icon("triangle-exclamation"),
            " Physiology defaults in this model — hematocrit (fixed at 0.3802) and the blood-volume scaling constant — are derived from adult ",
            shiny::tags$strong("male"),
            " / occupational-worker reference data (Leggett, 1993) and are not adjustable here. There is currently no female-specific parameter set (e.g., a lower hematocrit range or pregnancy-related blood-volume expansion); results should not be assumed accurate for female individuals."
          )
        ),

        # Time
        bslib::card(
          class = "card-tight",
          bslib::card_header(shiny::div(
            class = "section-title",
            shiny::icon("clock"),
            "Time Parameters"
          )),
          shiny::conditionalPanel(
            condition = "input['simulation.type'] != 'pulse'",
            add_tooltip(
              shiny::numericInput(
                "working.hours",
                shiny::tagList(
                  "Working Hours per Week",
                  shiny::span(class = "label-unit", shiny::tags$small("(h/week)"))
                ),
                value = 40,
                min = 0,
                max = 168,
                step = 1
              ),
              "Average weekly time at the occupational site."
            )
          ),
          add_tooltip(
            shiny::numericInput(
              "exposure.years",
              shiny::tagList(
                "Years of Exposure",
                shiny::span(class = "label-unit", shiny::tags$small("(years)"))
              ),
              value = 20,
              min = 0,
              step = 1
            ),
            "Span of the active exposure period."
          ),
          shiny::conditionalPanel(
            condition = "input['simulation.type'] == 'occupational_retirement'",
            add_tooltip(
              shiny::numericInput(
                "retired.years",
                shiny::tagList(
                  "Years of Retirement",
                  shiny::span(class = "label-unit", shiny::tags$small("(years)"))
                ),
                value = 20,
                min = 0,
                step = 1
              ),
              "Post-work period with reduced workplace exposure."
            )
          ),
          add_tooltip(
            shiny::numericInput(
              "starting.age",
              shiny::tagList(
                "Starting Age",
                shiny::span(class = "label-unit", shiny::tags$small("(years)"))
              ),
              value = 30,
              min = 0,
              step = 1
            ),
            "Age at the start of the simulation."
          )
        ),

        # Starting conditions
        bslib::card(
          class = "card-tight",
          bslib::card_header(shiny::div(
            class = "section-title",
            shiny::icon("vial"),
            "Starting Conditions"
          )),
          add_tooltip(
            shiny::numericInput(
              "initial.blood.level.ug.dL",
              shiny::tagList(
                "Initial Blood Lead Level",
                shiny::span(class = "label-unit", shiny::tags$small("(µg/dL)"))
              ),
              value = 0.78,
              min = 0,
              step = 0.1
            ),
            shiny::tagList(
              "Baseline BLL prior to new exposures. The 50th percentile value for the total US population between 2015-2016 is 0.78 µg/dL, according to the Centers for Disease Control and Prevention (2019)."
            )
          ),
          shiny::p(
            "Detailed summary statistics for blood lead level biomonitoring for the US population gathered between 1999 - 2016 is available from the Centers for Disease Control and Prevention",
            shiny::a(
              href = "https://stacks.cdc.gov/view/cdc/75822",
              "(CDC 2019).",
              target = "_blank"
            )
          )
        )
      ),

      # Row 3: Exposure card (auto-switches by scenario)
      bslib::card(
        bslib::card_header(shiny::div(
          class = "section-title",
          shiny::icon("cloud"),
          "Exposure Inputs"
        )),
        # Occupational / Retirement
        shiny::conditionalPanel(
          condition = "input['simulation.type'] != 'pulse'",
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              shiny::h4(shiny::icon("wind"), "Airborne Concentrations"),
              add_tooltip(
                shiny::numericInput(
                  "occ.airborne.concentration.ug.m3",
                  shiny::tagList(
                    "Occupational",
                    shiny::span(class = "label-unit", shiny::tags$small("(µg/m³)"))
                  ),
                  value = 30,
                  min = 0,
                  step = .1
                ),
                "Input the average airborne lead at the workplace. The default value of 30 µg/m³ is based on the OSHA action level (29 CFR 1910.1025(b)), and is not meant to represent all workplaces."
              ),
              add_tooltip(
                shiny::numericInput(
                  "Nonocc.airborne.concentration.ug.m3",
                  shiny::tagList(
                    "Non-Occupational",
                    shiny::span(class = "label-unit", shiny::tags$small("(µg/m³)"))
                  ),
                  value = 0.15,
                  min = 0,
                  step = .1
                ),
                "Input the average airborne lead outside the workplace. The default value of 0.15 µg/m³ is based on the National Ambient Air Quality Standard (rolling 3-month average)."
              )
            ),
            shiny::div(
              shiny::h4(shiny::icon("utensils"), "Daily Exposure Totals"),
              add_tooltip(
                shiny::numericInput(
                  "total.occ.exposure",
                  shiny::tagList(
                    "Occupational",
                    shiny::span(class = "label-unit", shiny::tags$small("(µg/day)"))
                  ),
                  value = 25,
                  min = 0,
                  step = .1
                ),
                "Total daily intake from all occupational sources (e.g., inhalation, diet, dermal contact, etc.). Note that this amount will be added to the estimated total from airborne exposure. A value of 25 µg/day is used as a default - and is not meant to represent all workplaces."
              ),
              add_tooltip(
                shiny::numericInput(
                  "total.non.occupational.exposure",
                  shiny::tagList(
                    "Non-Occupational",
                    shiny::span(class = "label-unit", shiny::tags$small("(µg/day)"))
                  ),
                  value = 2.2,
                  min = 0,
                  step = .1
                ),
                "Input the total daily intake from all non-occupational sources (e.g., inhalation, diet, dermal contact, etc.). Note that this amount will be added to the estimated total from airborne exposure. For reference, the US Food and Drug Administration has set an interim reference level of 2.2 µg/day from food for children (FDA 2022)."
              )
            )
          ),
          shiny::div(
            class = "mt-3",
            shiny::h4(shiny::icon("arrows-rotate"), "Worker Adjustment / Discount Factor (WAF/DF)"),
            add_tooltip(
              shiny::checkboxInput(
                "use_waf",
                "Adjust occupational air using Office of Environmental Health Hazard Assessment WAF/DF (see About page for details)",
                value = FALSE
              ),
              "Adjust annual average to a shift concentration when sources are non-continuous. Calculated automatically based on user input below. See the About page for more information."
            ),
            shiny::conditionalPanel(
              condition = "input.use_waf",
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::numericInput(
                    "Hsource",
                    "Source hours/day",
                    value = 8,
                    min = 1,
                    max = 24,
                    step = 1
                  ),
                  shiny::numericInput(
                    "Dsource",
                    "Source days/week",
                    value = 5,
                    min = 1,
                    max = 7,
                    step = 1
                  )
                ),
                shiny::column(
                  6,
                  shiny::numericInput(
                    "Hworker",
                    "Worker hours/day",
                    value = 8,
                    min = 1,
                    max = 24,
                    step = 1
                  ),
                  shiny::numericInput(
                    "Dworker",
                    "Worker days/week",
                    value = 5,
                    min = 1,
                    max = 7,
                    step = 1
                  ),
                  shiny::checkboxInput(
                    "use_df",
                    "Use Discount Factor (partial overlap)",
                    value = FALSE
                  ),
                  shiny::conditionalPanel(
                    condition = "input.use_df",
                    shiny::numericInput(
                      "Hcoincident",
                      "Coincident hours/day",
                      value = 8,
                      min = 0,
                      max = 24,
                      step = 1
                    ),
                    shiny::numericInput(
                      "Dcoincident",
                      "Coincident days/week",
                      value = 5,
                      min = 0,
                      max = 7,
                      step = 1
                    )
                  )
                )
              ),
              shiny::htmlOutput("waf_echo")
            )
          )
        ),

        # Pulse
        shiny::conditionalPanel(
          condition = "input['simulation.type'] == 'pulse'",
          shiny::div(
            class = "subtle mb-2",
            shiny::icon("bolt"),
            shiny::strong("Pulse mode"),
            " — add discrete exposure spikes during the simulation window."
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            add_tooltip(
              shiny::numericInput(
                "pulse.amount",
                shiny::tagList(
                  "Pulse Dose",
                  shiny::span(class = "label-unit", shiny::tags$small("(µg)"))
                ),
                value = 1,
                min = 0,
                step = .1
              ),
              "Lead mass delivered by each pulse."
            ),
            add_tooltip(
              shiny::numericInput(
                "pulse.count",
                shiny::tagList(
                  "Number of Pulses",
                  shiny::span(class = "label-unit", shiny::tags$small("(count)"))
                ),
                value = 1,
                min = 1,
                step = 1
              ),
              "How many pulses to simulate."
            ),
            add_tooltip(
              shiny::numericInput(
                "pulse.interval",
                shiny::tagList(
                  "Pulse Interval",
                  shiny::span(class = "label-unit", shiny::tags$small("(days)"))
                ),
                value = 30,
                min = 1,
                step = 1
              ),
              "Spacing between pulse events."
            )
          )
        )
      ),

      # Sticky run bar
      shiny::div(
        class = "sticky-run mt-3",
        add_tooltip(
          shiny::actionButton(
            "simulate",
            label = shiny::tagList(
              shiny::icon("rocket"),
              shiny::span(style = "font-size:1.25rem;", "Initiate Simulation")
            ),
            width = "100%",
            style = "height:56px; font-weight:700;"
          ),
          "Run the simulation using the current scenario configuration."
        )
      )
    )
  ),

  # -------- Model Parameters --------
  #  shiny::tabPanel(
  #    title = "Model Parameters",
  #    value = "tab_model_parameters",
  #    icon = shiny::icon("code"),
  #    shiny::fluidPage(
  #      shiny::h1("Model Parameters"),
  #      shiny::p(class="subtle",
  #        "(Advanced) — These physiological parameters are modular. ",
  #        "Change only if you are familiar with the model."
  #      ),
  #      DT::DTOutput("Model.Parameters")
  #    )
  #  ),

  # -------- Results --------
  shiny::tabPanel(
    title = "Results",
    value = "tab_results",
    icon = shiny::icon("chart-simple"),
    shiny::fluidPage(
      shiny::br(),
      shiny::br(),
      shiny::h1("Results"),
      shiny::uiOutput("scenario_badge"),

      # Collapsible left-hand sidebar
      shiny::div(
        id = "results-layout",
        bslib::layout_sidebar(
          fillable = TRUE,
          collapsible = TRUE, # <- this gives you a toggle button
          open = TRUE, # default open
          title = "Legend & Filters",
          border = TRUE,

          # ---- LEFT: Legend & Filters (sidebar) ----
          sidebar = shiny::tagList(
            bslib::accordion(
              open = "legend_filters",
              bslib::accordion_panel(
                title = shiny::tagList(shiny::icon("filter"), "Legend & Filters"),
                value = "legend_filters",

                shiny::div(
                  class = "checkbox-group",
                  shiny::checkboxGroupInput(
                    inputId = "compartment_group_selection",
                    label = "Compartment groups",
                    choices = names(compartment_groups),
                    selected = names(compartment_groups),
                    inline = FALSE
                  )
                ),

                shiny::textInput(
                  "compartment_search",
                  label = "Search compartments (optional)",
                  placeholder = "e.g., Plasma, Liver, Bone…"
                ),

                shiny::uiOutput("compartment_picker"),

                shiny::div(
                  class = "download-controls mt-2",
                  shiny::downloadButton(
                    "download_time_series",
                    "Download Time-Series Data"
                  ),
                  shiny::downloadButton(
                    "download_time_plot",
                    "Download Interactive Plot"
                  )
                )
              )
            )
          ),

          # ---- RIGHT: Plot + Controls (main body) ----
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex justify-content-between align-items-center",
                shiny::div(
                  class = "section-title",
                  shiny::icon("chart-line"),
                  "Time-Point Simulation"
                ),
                shiny::downloadButton(
                  outputId = "download_plot_html",
                  label = shiny::tagList("Download Plotly (HTML)"),
                  class = "btn btn-primary btn-sm"
                )
              )
            ),
            shiny::div(
              class = "subtle mb-2",
              shiny::tags$span(class = "pill", shiny::icon("calendar"), "Time series"),
              shiny::tags$span(" • "),
              shiny::tags$span(
                class = "pill",
                shiny::icon("flask"),
                "Compartment values (µg)"
              )
            ),
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::checkboxInput(
                  "facet_by_group",
                  label = "Facet by group",
                  value = FALSE
                )
              ),
              shiny::column(
                4,
                shiny::checkboxInput(
                  "show_legend",
                  label = "Show legend",
                  value = TRUE
                )
              ),
              shiny::column(
                4,
                shiny::checkboxInput("log_y", label = "Log scale (µg)", value = TRUE)
              )
            ),
            shinycssloaders::withSpinner(plotly::plotlyOutput(
              outputId = "time.plotly",
              height = 800,
              width = "100%"
            ))
          ),

          # Summary card
          bslib::card(
            class = "mt-3",
            bslib::card_header(shiny::div(
              class = "section-title",
              shiny::icon("table"),
              "Summary Data"
            )),
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::numericInput(
                  inputId = "time.point",
                  label = "Time Point (Days)",
                  value = 500,
                  min = 0
                )
              )
            ),
            shinycssloaders::withSpinner(DT::DTOutput(
              outputId = "summary.table"
            ))
          ),
          bslib::card(
            class = "mt-3",
            bslib::card_header(shiny::div(
              class = "section-title",
              shiny::icon("clipboard-check"),
              "Breathing-Rate Audit"
            )),
            shiny::p(
              class = "subtle",
              "Echo of selected Office of Environmental Health Hazard Assessment (OEHHA) rows, conversions, and values actually fed to the solver."
            ),
            shinycssloaders::withSpinner(DT::DTOutput("br_audit_table"))
          )
        )
      )
    )
  ),

  # -------- About --------
  shiny::tabPanel(
    title = "About",
    value = "tab_about",
    icon = shiny::icon("info-circle"),
    shiny::fluidPage(
      shiny::br(),
      shiny::br(),
      shiny::h1("About the Leggett+ Lead PBPK Model"),

      # Overview + intro sections (left) with logo (right)
      shiny::fluidRow(
        shiny::column(
          8,
          # Intro badges
          shiny::div(
            class = "badge-wrap",
            shiny::span(class = "pill", shiny::icon("dna"), "Physiologically Based PK"),
            shiny::span(class = "pill", shiny::icon("wind"), "Inhalation & GI Pathways"),
            shiny::span(class = "pill", shiny::icon("cogs"), "Occupational Scenarios")
          ),
          shiny::p(
            "Leggett+ is the Office of Environmental Health Hazard Assessment (OEHHA)'s update to Richard W. Leggett's 1993 age-specific model of lead kinetics in humans.",
            "It preserves the modular multi-compartment structure (systemic, respiratory, and GI modules) while ",
            "revising selected adult parameters to better match observations from chronically exposed workers."
          ),
          shiny::h2(
            class = "section-title",
            shiny::icon("triangle-exclamation"),
            "Intended Use and Limitations"
          ),
          shiny::p(
            class = "subtle",
            "This tool is intended for research, regulatory evaluation, and exposure-scenario exploration.",
            "Outputs should be interpreted within the context of model assumptions, parameter uncertainty,",
            "and available toxicokinetic data. It is not a risk model and is not intended for clinical",
            "diagnosis or individual medical decision-making."
          ),
          shiny::p(
            class = "subtle",
            shiny::tags$strong("Male-reference physiology: "),
            "Leggett+'s physiological parameters (body weight, hematocrit, blood-volume scaling, and RBC binding capacity) are derived from adult ",
            shiny::tags$strong("male"),
            " and male-dominated occupational-worker cohorts (Leggett, 1993, and subsequent updates). The model contains near-total absence of female-specific parameterization — there is no separate hematocrit range, blood-volume expansion, or RBC binding capacity for female individuals, including during pregnancy. Simulated results should not be assumed to represent female physiology."
          ),
          shiny::h2(
            class = "section-title",
            shiny::icon("info-circle"),
            "What Leggett+ Simulates"
          ),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::tags$strong("Systemic kinetics:"),
              " distribution and retention in plasma/RBCs, soft tissues, liver, kidney, brain, and multiple trabecular/cortical bone pools."
            ),
            shiny::tags$li(
              shiny::tags$strong("Respiratory & GI uptake:"),
              " deposition and clearance in the respiratory tract with subsequent absorption to blood, plus GI absorption."
            ),
            shiny::tags$li(
              shiny::tags$strong("Occupational exposure:"),
              " workplace air–blood relationships via an inhalation transfer coefficient (ITC), combined with background intake."
            )
          )
        ),
        shiny::column(
          4,
          class = "about-image-wrapper",
          shiny::img(
            src = "leggettplus.png",
            alt = "LeggettPlus logo",
            style = "max-width: 286px; width: 100%; height: auto;"
          )
        )
      ),

      shiny::h2(
        class = "section-title",
        shiny::icon("magic"),
        "Key Updates vs. Leggett (1993)"
      ),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$strong("RBC binding revised:"),
          " removes the low-BLL threshold and aligns binding capacity with contemporary datasets."
        ),
        shiny::tags$li(
          shiny::tags$strong("Urinary pathway reduced:"),
          " lower plasma→bladder and urine-path rates slow post-exposure BLL decline."
        ),
        shiny::tags$li(
          shiny::tags$strong("Bone exchange updated:"),
          " longer exchange↔non-exchange half-lives (years) in trabecular/cortical bone to match worker data."
        ),
        shiny::tags$li(
          shiny::tags$strong("Workplace exposure module:"),
          " integrates respiratory intake/uptake via an ITC calibrated around 0.30 with literature-based deposition/clearance."
        )
      ),

      # --- Small callout + expandable ITC table ---
      shiny::h2(
        class = "section-title",
        shiny::icon("wind"),
        "Inhalation Transfer Coefficient (ITC)"
      ),
      shiny::div(
        class = "badge-wrap",
        shiny::span(
          class = "badge",
          style = "background:#eef3ff; color:#0d47a1; border:1px solid #d9e2ff;",
          shiny::icon("tachometer-alt"),
          "Default ITC: 0.30"
        ),
        shiny::span(class = "subtle", "Click below to see particle-size guidance")
      ),

      # Prefer bslib accordion; fallback to <details> if unavailable/old
      if (
        requireNamespace("bslib", quietly = TRUE) &&
          utils::packageVersion("bslib") >= "0.5.0"
      ) {
        bslib::accordion(
          bslib::accordion_panel(
            title = shiny::tagList(
              shiny::icon("list"),
              "ITC guidance by particle size (indicative)"
            ),
            value = "itc-guidance", # <-- explicit value fixes the error
            class = "card-tight",
            shiny::p(
              class = "subtle",
              "ITC represents the fraction of inhaled lead mass that ultimately transfers to blood. ",
              "Values depend on aerodynamic diameter and breathing pattern; ranges below are indicative and should be refined for your scenario."
            ),
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$thead(
                shiny::tags$tr(
                  shiny::tags$th("Aerodynamic diameter (µm)"),
                  shiny::tags$th("Typical breathing"),
                  shiny::tags$th("Dominant deposition"),
                  shiny::tags$th("Suggested ITC range"),
                  shiny::tags$th("Notes")
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td("≤ 0.1"),
                  shiny::tags$td("Nasal/oronasal, rest"),
                  shiny::tags$td("Alveolar"),
                  shiny::tags$td("0.25 – 0.35"),
                  shiny::tags$td("High alveolar penetration; slower clearance")
                ),
                shiny::tags$tr(
                  shiny::tags$td("0.1 – 1"),
                  shiny::tags$td("Oronasal, light work"),
                  shiny::tags$td("Alveolar"),
                  shiny::tags$td("0.30 – 0.45"),
                  shiny::tags$td("Peak total deposition; efficient uptake")
                ),
                shiny::tags$tr(
                  shiny::tags$td("1 – 5"),
                  shiny::tags$td("Oronasal, moderate work"),
                  shiny::tags$td("TB + Alveolar"),
                  shiny::tags$td("0.20 – 0.35"),
                  shiny::tags$td("More proximal deposition; mixed clearance")
                ),
                shiny::tags$tr(
                  shiny::tags$td("> 5"),
                  shiny::tags$td("Nasal/oronasal, variable"),
                  shiny::tags$td("Extrathoracic/TB"),
                  shiny::tags$td("0.10 – 0.25"),
                  shiny::tags$td("Greater upper-airway loss; less systemic uptake")
                )
              )
            ),
            shiny::p(
              class = "muted",
              shiny::icon("exclamation-triangle"),
              " These ranges are for orientation and may vary with hygroscopic growth, shape factor, and activity pattern."
            )
          )
        )
      } else {
        shiny::tags$details(
          shiny::tags$summary(shiny::tagList(
            shiny::icon("list"),
            shiny::span("ITC guidance by particle size (indicative)")
          )),
          shiny::div(
            class = "card-tight",
            shiny::p(
              class = "subtle",
              "ITC represents the fraction of inhaled lead mass that ultimately transfers to blood. ",
              "Values depend on aerodynamic diameter and breathing pattern; ranges below are indicative and should be refined for your scenario."
            ),
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$thead(
                shiny::tags$tr(
                  shiny::tags$th("Aerodynamic diameter (µm)"),
                  shiny::tags$th("Typical breathing"),
                  shiny::tags$th("Dominant deposition"),
                  shiny::tags$th("Suggested ITC range"),
                  shiny::tags$th("Notes")
                )
              ),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td("≤ 0.1"),
                  shiny::tags$td("Nasal/oronasal, rest"),
                  shiny::tags$td("Alveolar"),
                  shiny::tags$td("0.25 – 0.35"),
                  shiny::tags$td("High alveolar penetration; slower clearance")
                ),
                shiny::tags$tr(
                  shiny::tags$td("0.1 – 1"),
                  shiny::tags$td("Oronasal, light work"),
                  shiny::tags$td("Alveolar"),
                  shiny::tags$td("0.30 – 0.45"),
                  shiny::tags$td("Peak total deposition; efficient uptake")
                ),
                shiny::tags$tr(
                  shiny::tags$td("1 – 5"),
                  shiny::tags$td("Oronasal, moderate work"),
                  shiny::tags$td("TB + Alveolar"),
                  shiny::tags$td("0.20 – 0.35"),
                  shiny::tags$td("More proximal deposition; mixed clearance")
                ),
                shiny::tags$tr(
                  shiny::tags$td("> 5"),
                  shiny::tags$td("Nasal/oronasal, variable"),
                  shiny::tags$td("Extrathoracic/TB"),
                  shiny::tags$td("0.10 – 0.25"),
                  shiny::tags$td("Greater upper-airway loss; less systemic uptake")
                )
              )
            ),
            shiny::p(
              class = "muted",
              shiny::icon("exclamation-triangle"),
              " These ranges are for orientation and may vary with hygroscopic growth, shape factor, and activity pattern."
            )
          )
        )
      },

      # --- Collapsible model assumptions & limitations ---
      shiny::h2(
        class = "section-title",
        shiny::icon("balance-scale"),
        "Model Assumptions & Limitations"
      ),
      if (
        requireNamespace("bslib", quietly = TRUE) &&
          utils::packageVersion("bslib") >= "0.5.0"
      ) {
        bslib::accordion(
          bslib::accordion_panel(
            title = shiny::tagList(shiny::icon("list"), "Key assumptions"),
            value = "assumptions", # <-- explicit value
            class = "card-tight",
            shiny::tags$ul(
              shiny::tags$li(
                "Daily-step ODE solution; parameters treated as fixed within a simulation run."
              ),
              shiny::tags$li(
                "Age-specific physiology; adult worker parameterization emphasized in Leggett+."
              ),
              shiny::tags$li(
                shiny::tags$strong("Male-reference physiology: "),
                "body weight, hematocrit (fixed at 0.3802), and blood-volume scaling are derived from adult male / occupational-worker data; no female-specific parameter set is implemented."
              ),
              shiny::tags$li("RBC binding function without a low-BLL threshold."),
              shiny::tags$li(
                "Respiratory module lumps deposition, mucociliary clearance, and absorption into an effective ITC."
              ),
              shiny::tags$li(
                "Non-occupational intake represented as daily totals (not micro-environmental time-activity)."
              )
            )
          ),
          bslib::accordion_panel(
            title = shiny::tagList(shiny::icon("exclamation-triangle"), "Limitations"),
            value = "limitations", # <-- explicit value
            class = "card-tight",
            shiny::tags$ul(
              shiny::tags$li(
                "Not a risk model—outputs are kinetic states (e.g., µg in compartments, BLL), not health outcomes."
              ),
              shiny::tags$li(
                "Particle size and breathing pattern strongly influence uptake; ITC uncertainty propagates to BLL."
              ),
              shiny::tags$li(
                "Bone exchange kinetics imply multi-year equilibration; short studies may not capture long-term declines."
              ),
              shiny::tags$li(
                "Parameter values reflect literature averages; individual variability can be substantial."
              )
            )
          )
        )
      } else {
        shiny::tagList(
          shiny::tags$details(
            shiny::tags$summary(shiny::tagList(shiny::icon("list"), shiny::span("Key assumptions"))),
            shiny::div(
              class = "card-tight",
              shiny::tags$ul(
                shiny::tags$li(
                  "Daily-step ODE solution; parameters treated as fixed within a simulation run."
                ),
                shiny::tags$li(
                  "Age-specific physiology; adult worker parameterization emphasized in Leggett+."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Male-reference physiology: "),
                  "body weight, hematocrit (fixed at 0.3802), and blood-volume scaling are derived from adult male / occupational-worker data; no female-specific parameter set is implemented."
                ),
                shiny::tags$li("RBC binding function without a low-BLL threshold."),
                shiny::tags$li(
                  "Respiratory module lumps deposition, mucociliary clearance, and absorption into an effective ITC."
                ),
                shiny::tags$li(
                  "Non-occupational intake represented as daily totals (not micro-environmental time-activity)."
                )
              )
            )
          ),
          shiny::tags$details(
            shiny::tags$summary(shiny::tagList(
              shiny::icon("exclamation-triangle"),
              shiny::span("Limitations")
            )),
            shiny::div(
              class = "card-tight",
              shiny::tags$ul(
                shiny::tags$li(
                  "Not a risk model—outputs are kinetic states (e.g., µg in compartments, BLL), not health outcomes."
                ),
                shiny::tags$li(
                  "Particle size and breathing pattern strongly influence uptake; ITC uncertainty propagates to BLL."
                ),
                shiny::tags$li(
                  "Bone exchange kinetics imply multi-year equilibration; short studies may not capture long-term declines."
                ),
                shiny::tags$li(
                  "Parameter values reflect literature averages; individual variability can be substantial."
                )
              )
            )
          )
        )
      },

      # References
      # --- Office of Environmental Health Hazard Assessment (OEHHA) Hot Spots Inhalation Guidance (2015) ---

      shiny::h2(
        class = "section-title",
        shiny::icon("book-open"),
        "Office of Environmental Health Hazard Assessment (OEHHA) Hot Spots Inhalation Guidance (2015)"
      ),
      shiny::p(
        class = "subtle",
        "This app can follow the Office of Environmental Health Hazard Assessment (OEHHA) Air Toxics Hot Spots Guidance (Feb 2015) for inhalation",
        " exposure point-estimates and worker shift adjustments. See citation at bottom."
      ),

      # TL;DR bullets
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("Point estimates (Tier 1): "),
          "Use ",
          shiny::tags$em("mean"),
          " for average and ",
          shiny::tags$em("95th percentile"),
          " for high-end."
        ),
        shiny::tags$li(
          shiny::tags$b("Avoid compounding conservatism: "),
          "High-end only for ",
          shiny::tags$em("driving pathways"),
          "; mean for others."
        ),
        shiny::tags$li(
          shiny::tags$b("Cancer durations: "),
          "9, 30 (MEIR default), and 70 years; exposure begins in 3rd trimester."
        ),
        shiny::tags$li(
          shiny::tags$b("Workers: "),
          "Use 8-hour breathing rates by activity (sedentary/light/moderate)."
        ),
        shiny::tags$li(
          shiny::tags$b("WAF/DF: "),
          "Adjust annual average to a shift concentration when sources are non-continuous."
        )
      ),

      # Equations (text chips)
      shiny::div(
        shiny::p(eq_chip(
          "Residential dose (Eq 5.4.1.1):  Dose = Cair × {BR/BW} × A × EF × 10^-6"
        )),
        shiny::p(eq_chip(
          "Worker dose (Eq 5.4.1.2A):     Dose = (Cair × WAF) × {BR/BW} × A × EF × 10^-6"
        )),
        shiny::p(eq_chip(
          "WAF (Eq 5.4.1.2B):             WAF = (Hres/Hsource) × (Dres/Dsource) × DF"
        )),
        shiny::p(eq_chip(
          "DF  (Eq 5.4.1.2C):             DF  = (Hcoincident/Hworker) × (Dcoincident/Dworker)"
        ))
      ),

      if (
        requireNamespace("bslib", quietly = TRUE) &&
          utils::packageVersion("bslib") >= "0.5.0"
      ) {
        bslib::accordion(
          bslib::accordion_panel(
            title = shiny::tagList(
              shiny::icon("table"),
              "Residential Daily Breathing Rates (Table 5.6, L/kg-day)"
            ),
            value = "guidance-res",
            class = "card-tight",
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th("Age band"),
                shiny::tags$th("Mean"),
                shiny::tags$th("95th")
              )),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td("3rd trimester"),
                  shiny::tags$td("225"),
                  shiny::tags$td("361")
                ),
                shiny::tags$tr(shiny::tags$td("0–<2 y"), shiny::tags$td("658"), shiny::tags$td("1090")),
                shiny::tags$tr(shiny::tags$td("2–<9 y"), shiny::tags$td("535"), shiny::tags$td("861")),
                shiny::tags$tr(shiny::tags$td("2–<16 y"), shiny::tags$td("452"), shiny::tags$td("745")),
                shiny::tags$tr(shiny::tags$td("16–<30 y"), shiny::tags$td("210"), shiny::tags$td("335")),
                shiny::tags$tr(shiny::tags$td("16–70 y"), shiny::tags$td("185"), shiny::tags$td("290"))
              )
            ),
            shiny::p(
              class = "muted",
              shiny::icon("info-circle"),
              "Use mean for average pathway inputs and 95th for driving pathways under Tier 1."
            )
          ),
          bslib::accordion_panel(
            title = shiny::tagList(
              shiny::icon("table"),
              "Worker 8-Hour Breathing Rates (Table 5.8, L/kg-8h)"
            ),
            value = "guidance-work",
            class = "card-tight",
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th("Activity (METS)"),
                shiny::tags$th("Mean"),
                shiny::tags$th("95th")
              )),
              shiny::tags$tbody(
                shiny::tags$tr(
                  shiny::tags$td("Sedentary/passive (<1.5)"),
                  shiny::tags$td("30"),
                  shiny::tags$td("40")
                ),
                shiny::tags$tr(
                  shiny::tags$td("Light (1.5–<3.0)"),
                  shiny::tags$td("80"),
                  shiny::tags$td("100")
                ),
                shiny::tags$tr(
                  shiny::tags$td("Moderate (3.0–<6.0)"),
                  shiny::tags$td("170"),
                  shiny::tags$td("230")
                )
              )
            ),
            shiny::p(
              class = "muted",
              shiny::icon("person-walking"),
              "Tier 1 default for adults is usually ",
              shiny::tags$strong("moderate, 95th"),
              ".",
              " See the app’s new Job Presets for quick mapping of typical roles."
            ),
            shiny::p(
              class = "muted",
              shiny::icon("baby-carriage"),
              shiny::tags$strong("Pregnancy: "),
              "For pregnant workers, the Office of Environmental Health Hazard Assessment (OEHHA) recommends using the 16-<30 y, moderate 8-hour row."
            )
          ),
          bslib::accordion_panel(
            title = shiny::tagList(
              shiny::icon("arrows-rotate"),
              "Worker Adjustment Factor / Discount Factor (WAF/DF)"
            ),
            value = "guidance-waf",
            class = "card-tight",
            shiny::p(
              "When sources are non-continuous (e.g., 8 h/day, 5 d/week), ",
              "use ",
              shiny::tags$strong("WAF"),
              " to convert the annual average model output to a work-shift concentration. ",
              "For partial shift overlap, apply a ",
              shiny::tags$strong("Discount Factor (DF)"),
              " for cancer assessments."
            ),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$code("Continuous (24/7): WAF = 1.0")),
              shiny::tags$li(shiny::tags$code(
                "8 h/day, 5 d/week with full overlap: WAF ≈ 4.2"
              )),
              shiny::tags$li(shiny::tags$code(
                "If only 4 of 8 h overlap: DF = 0.5 → WAF ≈ 2.1"
              ))
            ),
            shiny::p(
              class = "muted",
              shiny::icon("triangle-exclamation"),
              "For noncancer 8-hr hazards, WAF is used ",
              shiny::tags$em("without DF"),
              ". The app’s WAF block explains both behaviors."
            )
          ),
          bslib::accordion_panel(
            title = shiny::tagList(shiny::icon("children"), "Schools/Daycare & Durations"),
            value = "guidance-schools",
            class = "card-tight",
            shiny::tags$ul(
              shiny::tags$li(
                "School/daycare exposures can reuse the worker 8-hour framework with child age-bands."
              ),
              shiny::tags$li(
                "Cancer durations: 9, 30 (default for MEIR), and 70 years; exposure begins in 3rd trimester."
              ),
              shiny::tags$li("Population analyses generally use 70 years.")
            )
          )
        )
      } else {
        shiny::tagList(
          shiny::h3(shiny::icon("table"), "Residential Daily Breathing Rates (L/kg-day)"),
          # (simple fallback tables as above)
          shiny::p("…")
        )
      },

      # --- Codebase & R package ---
      shiny::h2(class = "section-title", shiny::icon("code-branch"), "Source Code & R Package"),
      shiny::p(
        class = "subtle",
        "This Shiny app is a thin interface over ",
        shiny::tags$code("LeggettPlus"),
        ", an open-source R package implementing the same 21-compartment model used here. The full codebase — app, package, and documentation — is available on GitHub:"
      ),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::a(
            href = "https://github.com/OEHHA-NTES/Leggett_Plus",
            target = "_blank",
            "github.com/OEHHA-NTES/Leggett_Plus"
          )
        )
      ),
      shiny::p(
        class = "subtle",
        shiny::tags$strong("For power users: "),
        "the ",
        shiny::tags$code("LeggettPlus"),
        " R package can be installed directly for scripted, batch, or reproducible use outside this app ",
        "(custom schedules, programmatic access to ",
        shiny::tags$code("run_leggett_model()"),
        " and ",
        shiny::tags$code("summarize_leggett_output()"),
        ", and integration into larger analysis pipelines)."
      ),
      shiny::tags$pre(
        'remotes::install_github("OEHHA-NTES/Leggett_Plus")\nlibrary(LeggettPlus)'
      ),

      # --- Updated References (include OEHHA PDF) ---
      shiny::h2(class = "section-title", shiny::icon("book"), "References"),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$em(
            "Air Toxics Hot Spots Program Guidance Manual for Preparation of Health Risk Assessments."
          ),
          " Office of Environmental Health Hazard Assessment (OEHHA), February 2015. ",
          shiny::a(
            href = "https://oehha.ca.gov/sites/default/files/media/downloads/crnr/2015gmappendicesaf.pdf",
            target = "_blank",
            "Appendices A–F (PDF)"
          ),
          " (Tables and equations cited above)."
        ),
        shiny::tags$li(
          shiny::tags$em("Adult systemic adjustments: "),
          shiny::a(
            href = "https://doi.org/10.1080/15459624.2020.1743845",
            target = "_blank",
            "Vork & Carlisle (2020), JOEH 17(4)"
          )
        ),
        shiny::tags$li(
          shiny::tags$em("Workplace exposure & ITC: "),
          shiny::a(
            href = "https://doi.org/10.1080/15459624.2022.2150767",
            target = "_blank",
            "Vork, Brown & Carlisle (2023), JOEH 20(2)"
          )
        )
      ),

      shiny::div(
        class = "about-image-wrapper",
        shiny::img(
          src = "model.jpg",
          alt = "Leggett+ systemic and exposure modules diagram"
        )
      )
    )
  ),
  ####### Additional OEHHA Apps #####
  # tabPanel(
  #   title = "Additional OEHHA Apps",
  #   value = "tab_additional_apps",
  #   icon = icon("table-cells"),
  #   fluidPage(
  #     br(),
  #     br(),
  #     h1("Explore Additional OEHHA Applications"),
  #     p(
  #       class = "subtle",
  #       "OEHHA provides interactive applications for exploring environmental and public health data.",
  #       " Browse featured tools below."
  #     ),

  #     # Page intro card
  #     bslib::card(
  #       class = "card-tight",
  #       bslib::card_header(div(
  #         class = "section-title",
  #         icon("compass"),
  #         "Featured Applications"
  #       )),
  #       p("Links, brief descriptions, and repositories for selected tools.")
  #     ),

  #     # App cards in a responsive grid
  #     bslib::layout_columns(
  #       col_widths = c(6, 6),

  #       # --- App: OEHHA Chemical Data Explorer ---
  #       bslib::card(
  #         class = "card-tight",
  #         bslib::card_header(div(
  #           class = "section-title",
  #           icon("flask-vial"),
  #           "OEHHA Chemical Data Explorer"
  #         )),
  #         p(
  #           "Rapidly identify, visualize, and access chemical hazard, production, and exposure data."
  #         ),
  #         div(
  #           class = "mb-2",
  #           a(
  #             href = "https://oehha.shinyapps.io/OEHHA-Data-Explorer/",
  #             target = "_blank",
  #             class = "btn btn-primary",
  #             icon("link"),
  #             " Visit the Chemical Data Explorer Tool"
  #           ),
  #           HTML("&nbsp;"),
  #           a(
  #             href = "https://github.com/OEHHA-NTES/OEHHA-Data-Explorer",
  #             target = "_blank",
  #             class = "btn btn-outline-primary",
  #             icon("github"),
  #             " View GitHub Repository"
  #           )
  #         ),
  #         tags$a(
  #           href = "https://oehha.shinyapps.io/OEHHA-Data-Explorer/",
  #           target = "_blank",
  #           tags$img(
  #             src = "chemical_app_screenshot.png",
  #             alt = "Screenshot of OEHHA Chemical Data Explorer",
  #             style = "max-width:100%; border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.08);"
  #           )
  #         )
  #       ),

  #       # --- App: OEHHA PFAS TK Modelling App ---
  #       bslib::card(
  #         class = "card-tight",
  #         bslib::card_header(div(
  #           class = "section-title",
  #           icon("vial"),
  #           "OEHHA PFAS TK Modelling App"
  #         )),
  #         p(
  #           "Model serum concentrations of PFAS based on administered doses across several species."
  #         ),
  #         div(
  #           class = "mb-2",
  #           a(
  #             href = "https://oehha.shinyapps.io/PFAS_TK/",
  #             target = "_blank",
  #             class = "btn btn-primary",
  #             icon("link"),
  #             " Visit the PFAS TK Modelling App"
  #           ),
  #           HTML("&nbsp;"),
  #           a(
  #             href = "https://github.com/OEHHA-NTES/PFAS_TK_Shiny",
  #             target = "_blank",
  #             class = "btn btn-outline-primary",
  #             icon("github"),
  #             " View GitHub Repository"
  #           )
  #         ),
  #         tags$a(
  #           href = "https://oehha.shinyapps.io/PFAS_TK/",
  #           target = "_blank",
  #           tags$img(
  #             src = "PFAS_TK_screenshot.png",
  #             alt = "Screenshot of OEHHA PFAS TK Modelling App",
  #             style = "max-width:100%; border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.08);"
  #           )
  #         )
  #       ),

  #       # --- App: CalEnviroScreen ---
  #       bslib::card(
  #         class = "card-tight",
  #         bslib::card_header(div(
  #           class = "section-title",
  #           icon("map"),
  #           "CalEnviroScreen 4.0"
  #         )),
  #         p(
  #           "Maps and data on pollution burden and population characteristics across California to identify disadvantaged communities."
  #         ),
  #         div(
  #           class = "mb-2",
  #           a(
  #             href = "https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-40",
  #             target = "_blank",
  #             class = "btn btn-primary",
  #             icon("link"),
  #             " Visit the CalEnviroScreen 4.0 Tool"
  #           )
  #         ),
  #         tags$a(
  #           href = "https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-40",
  #           target = "_blank",
  #           tags$img(
  #             src = "calenviroscreen_screenshot.png",
  #             alt = "Screenshot of CalEnviroScreen 4.0",
  #             style = "max-width:100%; border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.08);"
  #           )
  #         )
  #       ),
  #     # --- App: OrBit ---
  #       bslib::card(
  #         class = "card-tight",
  #         bslib::card_header(div(
  #           class = "section-title",
  #           icon("flask-vial"),
  #           "OEHHA Resource for Benchmark Inference Tool (OrBit)"
  #         )),
  #         p(
  #           "Conduct batch, probabilistic benchmark dose modeling using the OEHHA Resource for Benchmark Inference Tool."
  #         ),
  #         div(
  #           class = "mb-2",
  #           a(
  #             href = "https://oehha.shinyapps.io/orbit_v1/",
  #             target = "_blank",
  #             class = "btn btn-primary",
  #             icon("link"),
  #             " Visit OrBit"
  #           ),
  #           HTML("&nbsp;"),
  #           a(
  #             href = "https://github.com/OEHHA-NTES/Dose_Response_with_ToxicR",
  #             target = "_blank",
  #             class = "btn btn-outline-primary",
  #             icon("github"),
  #             " View GitHub Repository"
  #           )
  #         ),
  #         tags$a(
  #           href = "https://oehha.shinyapps.io/orbit_v1/",
  #           target = "_blank",
  #           tags$img(
  #             src = "orbit_app_screenshot.png",
  #             alt = "Screenshot of OEHHA Resource for Benchmark Inference Tool (OrBit)",
  #             style = "max-width:100%; border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.08);"
  #           )
  #         )
  #       )
  #     ),

  #     # This app's repo (no self-link to the app)
  #     bslib::card(
  #       class = "card-tight mt-3",
  #       bslib::card_header(div(
  #         class = "section-title",
  #         icon("code-branch"),
  #         "Lead Leggett+ PBPK Modelling App (This App)"
  #       )),
  #       p(
  #         "Questions or comments about this app? Please visit our GitHub repository to look under the hood at the model code, access documentation, and let us know about any issues."
  #       ),
  #       div(
  #         class = "mb-2",
  #         a(
  #           href = "https://github.com/OEHHA-NTES/Leggett_R_Shiny",
  #           target = "_blank",
  #           class = "btn btn-outline-primary",
  #           icon("github"),
  #           " View GitHub Repository"
  #         )
  #       )
  #     )
  #   )
  # )
)



server <- function(input, output, session) {
  output$scenario_description <- shiny::renderUI({
    type <- shiny::req(input$simulation.type)
    description <- scenario_descriptions[[type]]
    if (is.null(description)) {
      return(NULL)
    }
    shiny::tags$p(description)
  })

  output$scenario_badge <- shiny::renderUI({
    type <- shiny::req(input$simulation.type)
    badge <- switch(
      type,
      occupational = shiny::div(
        class = "badge badge-occ",
        shiny::icon("hard-hat"),
        "Occupational"
      ),
      occupational_retirement = shiny::div(
        class = "badge badge-ret",
        shiny::icon("person-cane"),
        "Occupational + Retirement"
      ),
      pulse = shiny::div(class = "badge badge-pulse", shiny::icon("bolt"), "Pulse"),
      shiny::div(class = "badge", shiny::icon("question"), "Unknown")
    )

    # Optional: quick stats once a sim exists
    stats_ui <- NULL
    if (!is.null(shiny::isolate(tryCatch(simulation(), error = function(e) NULL)))) {
      res <- simulation()
      max_t <- max(res$time_series$time, na.rm = TRUE)
      stats_ui <- shiny::span(
        class = "muted",
        "• Duration:",
        scales::comma(max_t),
        "days"
      )
    }

    shiny::div(class = "badge-wrap", badge, stats_ui)
  })

  output$compartment_picker <- shiny::renderUI({
    # Which groups are selected?
    sel_groups <- input$compartment_group_selection
    shiny::req(sel_groups)

    # Map selected groups -> compartments (by pretty label)
    in_groups <- compartment_palette |>
      dplyr::filter(general_group %in% sel_groups) |>
      dplyr::pull(compartment) |>
      unique()

    # Optional search filter
    srch <- input$compartment_search
    if (!is.null(srch) && nzchar(srch)) {
      pat <- tolower(srch)
      in_groups <- in_groups[grepl(pat, tolower(in_groups), fixed = TRUE)]
    }

    # Default select all visible (preserve previous selection if possible)
    prev <- shiny::isolate(input$compartment_selection)
    selected <- if (!is.null(prev)) intersect(prev, in_groups) else in_groups

    shiny::checkboxGroupInput(
      inputId = "compartment_selection",
      label = "Visible compartments",
      choices = in_groups,
      selected = selected,
      inline = FALSE
    )
  })

  # Apply job presets to intensity/percentile when user chooses a preset
  shiny::observeEvent(
    input$job_preset,
    {
      row <- JOB_PRESETS |> dplyr::filter(label == shiny::req(input$job_preset))
      if (nrow(row) == 1 && !is.na(row$intensity)) {
        shiny::updateSelectInput(
          session,
          "oehha_work_intensity",
          selected = row$intensity
        )
      }
      if (nrow(row) == 1 && !is.na(row$percentile)) {
        shiny::updateRadioButtons(session, "oehha_work_pct", selected = row$percentile)
      }
    },
    ignoreInit = TRUE
  )

  # Pregnancy guardrail (OEHHA note)
  output$pregnancy_hint <- shiny::renderUI({
    shiny::req(input$br_mode == "oehha")
    is_trimester <- identical(input$oehha_res_age, "3rd trimester")
    starts_worker_age <- !is.null(input$starting.age) &&
      is.finite(input$starting.age) &&
      input$starting.age >= 16 &&
      input$starting.age < 30
    if (is_trimester && starts_worker_age && input$simulation.type != "pulse") {
      shiny::div(
        class = "alert alert-warning mt-2",
        shiny::icon("exclamation-triangle"),
        shiny::strong(" Pregnancy guardrail: "),
        "For pregnant workers, the Office of Environmental Health Hazard Assessment (OEHHA) recommends using the ",
        shiny::em("16–<30 years, moderate-intensity 8-hour"),
        " worker breathing-rate row for work hours (Table 5.8)."
      )
    } else {
      NULL
    }
  })

  # ---- OEHHA BR calculators ----
  residential_br_m3h <- shiny::reactive({
    shiny::req(input$br_mode == "oehha")
    row <- OEHHA_BR_RES_DAILY_LKGD |> dplyr::filter(age_band == input$oehha_res_age)
    Lkgd <- if (shiny::req(input$oehha_res_pct) == "p95") row$p95 else row$mean
    Lkgd <- as.numeric(Lkgd)
    Lkgd_to_m3_per_h(Lkgd, bw_kg = shiny::req(input$body.weight))
  })

  work_br_m3h <- shiny::reactive({
    shiny::req(input$br_mode == "oehha")
    row <- OEHHA_BR_WORK_8H_LKG8H |>
      dplyr::filter(intensity == input$oehha_work_intensity)
    Lkg8h <- if (shiny::req(input$oehha_work_pct) == "p95") row$p95 else row$mean
    Lkg8h <- as.numeric(Lkg8h)
    Lkg8h_to_m3_per_h(Lkg8h, bw_kg = shiny::req(input$body.weight))
  })

  output$oehha_br_echo <- shiny::renderUI({
    shiny::req(input$br_mode == "oehha")
    occ <- signif(work_br_m3h(), 3)
    non <- signif(residential_br_m3h(), 3)
    shiny::tags$p(
      class = "subtle",
      shiny::icon("lungs"),
      shiny::HTML(sprintf(
        " Using Office of Environmental Health Hazard Assessment (OEHHA) tables (Appendix A, Table 5.8 for work; Table 5.6 for residential): <b>Occupational</b> approx <b>%s m3/h</b>, <b>Non-occupational</b> approx <b>%s m3/h</b> (auto-applied below).",
        occ,
        non
      ))
    )
  })

  waf_value <- shiny::reactive({
    shiny::req(input$use_waf)
    # Guardrails
    shiny::validate(
      shiny::need(
        input$Hsource > 0 && input$Hsource <= 24,
        "Source hours/day must be in (0,24]."
      ),
      shiny::need(
        input$Dsource > 0 && input$Dsource <= 7,
        "Source days/week must be in (0,7]."
      )
    )
    DF <- 1
    if (isTRUE(input$use_df)) {
      shiny::validate(
        shiny::need(
          input$Hworker > 0 && input$Hworker <= 24,
          "Worker hours/day must be in (0,24]."
        ),
        shiny::need(
          input$Dworker > 0 && input$Dworker <= 7,
          "Worker days/week must be in (0,7]."
        ),
        shiny::need(
          input$Hcoincident >= 0 && input$Hcoincident <= input$Hworker,
          "Coincident hours/day must be ≤ worker hours/day."
        ),
        shiny::need(
          input$Dcoincident >= 0 && input$Dcoincident <= input$Dworker,
          "Coincident days/week must be ≤ worker days/week."
        )
      )
      DF <- compute_DF(
        Hcoincident = input$Hcoincident,
        Hworker = input$Hworker,
        Dcoincident = input$Dcoincident,
        Dworker = input$Dworker
      )
    }
    compute_WAF(Hsource = input$Hsource, Dsource = input$Dsource, DF = DF)
  })

  output$waf_echo <- shiny::renderUI({
    shiny::req(input$use_waf)
    w <- waf_value()
    base <- input$occ.airborne.concentration.ug.m3
    adj <- base * w
    msg <- if (
      isTRUE(all.equal(input$Hsource, 24)) &&
        isTRUE(all.equal(input$Dsource, 7))
    ) {
      "Continuous source (24/7) → WAF = 1 (no change)."
    } else {
      NULL
    }
    shiny::tags$p(
      class = "subtle",
      shiny::icon("gauge-high"),
      shiny::HTML(sprintf(
        " WAF = <b>%0.3f</b> → adjusted occupational air = <b>%0.3f µg/m³</b> (from %0.3f). ",
        w,
        adj,
        base
      )),
      shiny::tags$br(),
      shiny::span(
        class = "muted",
        "Note: DF is a ",
        shiny::tags$em("cancer-style"),
        " overlap adjustment per Eq 5.4.1.2C; noncancer WAF does not apply DF."
      ),
      if (!is.null(msg)) shiny::tags$div(class = "text-muted", msg)
    )
  })

  # ---- Age-band / worker-span warning state for toast notifications ----
  age_band_warning_state <- shiny::reactive({
    shiny::req(input$br_mode == "oehha") # only applies to OEHHA-derived mode

    # Simulated age span
    start_age <- shiny::req(input$starting.age)
    exp_years <- as.numeric(input$exposure.years %||% 0)
    total_years <- exp_years +
      if (identical(input$simulation.type, "occupational_retirement")) {
        as.numeric(input$retired.years %||% 0)
      } else {
        0
      }
    end_age <- start_age + total_years

    # Residential age band parse/check
    band <- parse_age_band(input$oehha_res_age)
    within <- age_span_within_band(start_age, end_age, band)

    # Worker default span sanity check (adults)
    worker_min <- 16
    worker_max <- 70
    worker_flag <- (!is.finite(start_age)) ||
      start_age < worker_min ||
      end_age > worker_max

    # PRIORITY: mismatch (error) > trimester note (warn) > worker shiny::span(warn)
    if (!is.na(within) && identical(within, FALSE)) {
      rng <- if (isTRUE(band$upper_inclusive)) {
        sprintf("%g–%g y (inclusive)", band$min, band$max)
      } else {
        sprintf("%g–<%g y", band$min, band$max)
      }
      msg <- shiny::tagList(
        shiny::icon("triangle-exclamation"),
        shiny::strong(" Age-band mismatch: "),
        "The selected residential breathing-rate band (",
        shiny::em(input$oehha_res_age),
        ") spans ",
        rng,
        ", but your simulated age range is ",
        sprintf("%g–%g y. ", start_age, end_age),
        "Select a band that covers the span, or switch to Manual mode."
      )
      return(list(
        type = "error",
        ui = msg,
        key = paste(
          "mismatch",
          input$oehha_res_age,
          start_age,
          end_age,
          sep = "|"
        )
      ))
    }

    if (is.na(band$min) && grepl("^3rd", tolower(input$oehha_res_age))) {
      msg <- shiny::tagList(
        shiny::icon("circle-info"),
        shiny::strong(" 3rd trimester note: "),
        "This band reflects late-pregnancy (maternal respiration). ",
        "If your scenario spans multiple life stages, select an age band covering the span, ",
        "or use Manual mode with explicit m³/h."
      )
      return(list(
        type = "warning",
        ui = msg,
        key = paste("trimester", start_age, end_age, sep = "|")
      ))
    }

    if (input$simulation.type != "pulse" && worker_flag) {
      msg <- shiny::tagList(
        shiny::icon("person-walking"),
        shiny::strong(" Worker age out of range: "),
        "Office of Environmental Health Hazard Assessment (OEHHA) 8-hour worker breathing rates assume ages ",
        sprintf("%g–%g y. ", worker_min, worker_max),
        "Your simulated span is ",
        sprintf("%g–%g y. ", start_age, end_age),
        "For minors or >70 y, use Manual mode or a child/senior-appropriate framework."
      )
      return(list(
        type = "warning",
        ui = msg,
        key = paste("worker", start_age, end_age, sep = "|")
      ))
    }

    NULL
  })

  # ---- Toast controller (bottom-left) ----
  last_warn_key <- shiny::reactiveVal(NULL)
  active_notif_id <- shiny::reactiveVal(NULL)

  shiny::observeEvent(
    age_band_warning_state(),
    {
      st <- age_band_warning_state()

      # If no warning but one is displayed, remove it
      if (is.null(st)) {
        if (!is.null(active_notif_id())) {
          shiny::removeNotification(active_notif_id(), session = session)
          active_notif_id(NULL)
          last_warn_key(NULL)
        }
        return()
      }

      # If same state already shown, do nothing
      if (identical(st$key, last_warn_key())) {
        return()
      }

      # Replace any existing toast
      if (!is.null(active_notif_id())) {
        shiny::removeNotification(active_notif_id(), session = session)
        active_notif_id(NULL)
      }

      # Map type to showNotification type
      notif_type <- switch(
        st$type,
        "error" = "error",
        "warning" = "warning",
        "message"
      )

      id <- shiny::showNotification(
        ui = st$ui,
        type = notif_type,
        duration = 10, # seconds; set NULL for sticky
        closeButton = TRUE,
        session = session
      )
      active_notif_id(id)
      last_warn_key(st$key)
    },
    ignoreInit = TRUE
  )

  # Also clear toast when mode flips away from OEHHA
  shiny::observeEvent(
    input$br_mode,
    {
      if (!identical(input$br_mode, "oehha") && !is.null(active_notif_id())) {
        shiny::removeNotification(active_notif_id(), session = session)
        active_notif_id(NULL)
        last_warn_key(NULL)
      }
    },
    ignoreInit = TRUE
  )

  #
  # output$__alert_test <- shiny::renderUI({
  #   shiny::div(class="alert alert-danger", "If you see a red box, alerts render correctly.")
  # })

  build_base_stage <- function() {
    exposure_days <- max(1, round(input$exposure.years * 365))
    occ_background <- input$total.occ.exposure
    nonocc_background <- input$total.non.occupational.exposure
    occ_air <- input$occ.airborne.concentration.ug.m3
    nonocc_air <- input$Nonocc.airborne.concentration.ug.m3
    if (isTRUE(input$use_waf)) {
      occ_air <- occ_air * waf_value()
    }
    nonocc_breath_rate <- if (identical(input$br_mode, "oehha")) {
      residential_br_m3h()
    } else {
      input$Nonocc.breath.rate
    }
    occ_breath_rate <- if (identical(input$br_mode, "oehha")) {
      work_br_m3h()
    } else {
      input$occ.breath.rate
    }
    nonocc_breath_rate <- input$Nonocc.breath.rate
    work_hours <- input$working.hours

    if (input$simulation.type == "pulse") {
      occ_background <- 0
      nonocc_background <- 0
      occ_air <- 0
      nonocc_air <- 0
      occ_breath_rate <- 0
      nonocc_breath_rate <- 0
      work_hours <- 0
    }

    tibble::tibble(
      duration = exposure_days + 1,
      Occ.background.ug.d = occ_background,
      NonOcc.background.ug.d = nonocc_background,
      Occ.pb.ug.m3 = occ_air,
      NonOcc.pb.ug.m3 = nonocc_air,
      Occ.breath.rate.m3.d = occ_breath_rate * 24,
      NonOcc.breath.rate.m3.d = nonocc_breath_rate * 24,
      work.hour.perweek = work_hours,
      oral.intake.ug.d = 0,
      exposure_days = exposure_days
    )
  }

  build_pulse_schedule <- function(base_stage, total_days) {
    pulses <- max(1, round(input$pulse.count))
    interval <- max(1, round(input$pulse.interval))
    pulse_amount <- max(0, input$pulse.amount)

    schedule <- list()
    day_counter <- 0

    for (idx in seq_len(pulses)) {
      if (day_counter >= total_days) {
        break
      }
      pulse_stage <- base_stage
      pulse_stage$duration <- 1
      pulse_stage$Occ.background.ug.d <- pulse_stage$Occ.background.ug.d +
        pulse_amount
      schedule <- append(schedule, list(pulse_stage))
      day_counter <- day_counter + 1

      if (day_counter >= total_days) {
        break
      }

      if (idx < pulses) {
        gap_days <- min(interval - 1, total_days - day_counter)
        if (gap_days > 0) {
          gap_stage <- base_stage
          gap_stage$duration <- gap_days
          schedule <- append(schedule, list(gap_stage))
          day_counter <- day_counter + gap_days
        }
      }
    }

    if (day_counter < total_days) {
      remainder_stage <- base_stage
      remainder_stage$duration <- total_days - day_counter
      schedule <- append(schedule, list(remainder_stage))
    }

    dplyr::bind_rows(schedule)
  }

  build_stage_schedule <- function() {
    base_stage <- build_base_stage()

    exposure_days <- base_stage$exposure_days[1]
    base_stage <- base_stage |> dplyr::select(-exposure_days)

    if (input$simulation.type == "occupational") {
      base_stage
    } else if (input$simulation.type == "occupational_retirement") {
      work_stage <- base_stage
      work_stage$duration <- exposure_days + 1

      retired_days <- max(0, round(input$retired.years * 365))
      if (retired_days > 0) {
        retirement_stage <- base_stage
        retirement_stage$duration <- retired_days + 1
        retirement_stage$Occ.background.ug.d <- 0
        retirement_stage$Occ.pb.ug.m3 <- 0
        retirement_stage$work.hour.perweek <- 0
        dplyr::bind_rows(work_stage, retirement_stage)
      } else {
        work_stage
      }
    } else {
      total_days <- max(1, exposure_days)
      pulse_stage <- base_stage
      pulse_stage$duration <- 1
      build_pulse_schedule(pulse_stage, total_days)
    }
  }

  simulation <- shiny::eventReactive(input$simulate, {
    shiny::withProgress(message = "Running simulation...", value = 0, {
      shiny::incProgress(0.3, detail = "Configuring exposure schedule")
      schedule <- build_stage_schedule()
      shiny::incProgress(0.5, detail = "Solving compartment dynamics")
      result <- LeggettPlus::run_leggett_model(
        stage_schedule = schedule,
        body_weight_kg = input$body.weight,
        hematocrit = 0.3802,
        age_years = input$starting.age,
        initial_bll_ug_dl = input$initial.blood.level.ug.dL
      )
      shiny::incProgress(0.2, detail = "Finalizing outputs")
      result
    })
  })

  shiny::observeEvent(simulation(), {
    res <- simulation()
    max_time <- max(res$time_series$time)
    shiny::updateNumericInput(
      session,
      "time.point",
      min = 0,
      max = max_time,
      value = min(max_time, input$time.point)
    )
  })

  output$Model.Parameters <- DT::renderDT({
    res <- simulation()
    shiny::req(res)

    meta <- res$metadata
    stage_table <- meta$stage_schedule |>
      dplyr::mutate(Stage = paste0("Stage ", dplyr::row_number())) |>
      dplyr::select(Stage, dplyr::everything()) |>
      tidyr::pivot_longer(-Stage, names_to = "Parameter", values_to = "Value") |>
      dplyr::mutate(
        Parameter = dplyr::case_when(
          Parameter == "duration" ~ "Duration (days)",
          Parameter ==
            "Occ.background.ug.d" ~ "Occupational background (µg/day)",
          Parameter ==
            "NonOcc.background.ug.d" ~ "Non-occupational background (µg/day)",
          Parameter == "Occ.pb.ug.m3" ~ "Occupational airborne (µg/m³)",
          Parameter == "NonOcc.pb.ug.m3" ~ "Non-occupational airborne (µg/m³)",
          Parameter ==
            "Occ.breath.rate.m3.d" ~ "Occupational breathing (m³/day)",
          Parameter ==
            "NonOcc.breath.rate.m3.d" ~ "Non-occupational breathing (m³/day)",
          Parameter == "work.hour.perweek" ~ "Work hours per week",
          Parameter == "oral.intake.ug.d" ~ "Oral intake (µg/day)",
          TRUE ~ Parameter
        )
      )

    global_table <- tibble::tibble(
      Stage = "Global",
      Parameter = c(
        "Body weight (kg)",
        "Hematocrit (fixed, adult male reference)",
        "Age (years)",
        "Initial BLL (µg/dL)",
        "Blood volume (dL)",
        "Plasma volume (dL)",
        "RBC volume (dL)"
      ),
      Value = c(
        meta$body_weight_kg,
        meta$hematocrit,
        meta$age_years,
        meta$initial_bll_ug_dl,
        meta$blood_volume_dl,
        meta$plasma_volume_dl,
        meta$rbc_volume_dl
      )
    )

    param_table <- dplyr::bind_rows(global_table, stage_table)

    DT::datatable(
      param_table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        dom = 'Blrtip',
        buttons = list('copy', 'csv', 'excel')
      )
    ) |>
      DT::formatSignif(columns = "Value", digits = 6)
  })

  output$summary.table <- DT::renderDT({
    res <- simulation()
    shiny::req(res)

    summary_df <- LeggettPlus::summarize_leggett_output(
      res$time_series,
      input$time.point,
      res$metadata$blood_volume_dl,
      res$metadata$plasma_volume_dl,
      res$metadata$rbc_volume_dl
    )

    DT::datatable(
      summary_df,
      rownames = FALSE,
      extensions = 'Buttons',
      filter = "top",
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        width = '100%',
        scrollX = TRUE,
        dom = 'Blrtip',
        buttons = list(
          I('colvis'),
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(
                extend = "csv",
                filename = "page",
                exportOptions = list(
                  columns = ":visible",
                  modifier = list(page = "current")
                )
              ),
              list(
                extend = 'excel',
                filename = "page",
                title = NULL,
                exportOptions = list(
                  columns = ":visible",
                  modifier = list(page = "current")
                )
              )
            ),
            text = 'Download current page'
          ),
          list(
            extend = 'collection',
            buttons = list(
              list(
                extend = "csv",
                filename = "data",
                exportOptions = list(
                  columns = ":visible",
                  modifier = list(page = "all")
                )
              ),
              list(
                extend = 'excel',
                filename = "data",
                title = NULL,
                exportOptions = list(
                  columns = ":visible",
                  modifier = list(page = "all")
                )
              )
            ),
            text = 'Download all data'
          )
        ),
        lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All'))
      ),
      class = "display"
    ) |>
      DT::formatSignif(
        columns = names(summary_df)[names(summary_df) != "Compartment"],
        digits = 6
      )
  })

  time_plot_components <- shiny::reactive({
    res <- simulation()
    shiny::req(res)

    # ---- Filters ----
    sel_groups <- input$compartment_group_selection
    shiny::validate(shiny::need(
      length(sel_groups) > 0,
      "Select at least one compartment group."
    ))

    df <- res$time_series |>
      tidyr::pivot_longer(
        -time,
        names_to = "compartment_raw",
        values_to = "value"
      ) |>
      dplyr::left_join(
        compartment_palette |>
          dplyr::select(compartment_raw, compartment, general_group, color),
        by = "compartment_raw"
      ) |>
      dplyr::filter(
        !is.na(compartment_raw),
        !is.na(general_group),
        general_group %in% sel_groups
      )

    # optional search
    srch <- input$compartment_search
    if (!is.null(srch) && nzchar(srch)) {
      pat <- tolower(srch)
      df <- df |>
        dplyr::filter(grepl(pat, tolower(compartment %||% ""), fixed = TRUE))
    }
    # optional picker
    if (
      !is.null(input$compartment_selection) &&
        length(input$compartment_selection) > 0
    ) {
      df <- df |> dplyr::filter(compartment %in% input$compartment_selection)
    }

    shiny::validate(shiny::need(
      nrow(df) > 0,
      "Selected filters resulted in no visible compartments."
    ))

    # ---- Log handling ----
    if (isTRUE(input$log_y)) {
      df <- df |> dplyr::filter(is.finite(value), value > 0)
    } else {
      df <- df |> dplyr::filter(is.finite(value))
    }

    # ---- Ensure ALL KEYS are character (avoid factor indexing bugs) ----
    df <- df |>
      dplyr::mutate(
        compartment_raw = as.character(compartment_raw),
        compartment = as.character(compartment),
        general_group = as.character(general_group)
      )

    present_raw <- unique(df$compartment_raw)

    # Build maps (character-keyed!)
    base_color_map <- stats::setNames(
      as.character(compartment_palette$color),
      as.character(compartment_palette$compartment_raw)
    )
    base_label_map <- stats::setNames(
      as.character(compartment_palette$compartment),
      as.character(compartment_palette$compartment_raw)
    )

    # Reduce to present IDs, keep names
    col_now <- base_color_map[present_raw]
    lab_now <- base_label_map[present_raw]

    # Fallbacks (so nothing is NA → black)
    if (anyNA(col_now)) {
      miss <- which(is.na(col_now))
      col_now[miss] <- grDevices::hcl.colors(length(miss), "Dark3")
    }
    lab_now[is.na(lab_now)] <- present_raw[is.na(lab_now)]

    # ---- Title ----
    plot_title <- switch(
      input$simulation.type,
      occupational = "Occupational exposure scenario",
      occupational_retirement = "Occupational exposure with retirement",
      pulse = "Pulse exposure scenario",
      "Exposure scenario"
    )

    # ---- Panel builder: EXPLICIT per-trace colors (character indexing only) ----
    make_panel <- function(d, show_leg) {
      ids <- unique(d$compartment_raw) # character vector
      p <- plotly::plot_ly()

      use_gl <- (nrow(d) > 50000 || length(ids) > 60)

      for (id in ids) {
        sub <- d[d$compartment_raw == id, , drop = FALSE]
        line_col <- unname(col_now[id]) # id is character -> named lookup
        trace_nm <- unname(lab_now[id])

        # ultimate guard: if still NA, pick a visible fallback
        if (is.na(line_col) || !nzchar(line_col)) {
          line_col <- "#999999"
        }

        p <- p |>
          plotly::add_trace(
            data = sub,
            x = ~time,
            y = ~value,
            type = if (use_gl) "scattergl" else "scatter",
            mode = "lines",
            name = trace_nm,
            legendgroup = id,
            showlegend = isTRUE(show_leg),
            hovertemplate = paste0(
              "<b>",
              trace_nm,
              "</b>",
              "<br>Days: %{x}",
              "<br>\u00B5g: %{y:,}",
              "<extra></extra>"
            ),
            line = list(color = line_col, width = 1)
          )
      }

      p <- p |>
        plotly::layout(
          yaxis = list(
            title = "\u00B5g in compartment",
            type = if (isTRUE(input$log_y)) "log" else "linear"
          ),
          xaxis = list(title = "Days"),
          showlegend = isTRUE(input$show_legend)
        )

      # Retirement marker
      if (
        input$simulation.type == "occupational_retirement" &&
          nrow(res$metadata$stage_schedule) >= 2
      ) {
        work_duration <- res$metadata$stage_schedule$duration[1]
        retirement_start <- work_duration - 1
        if (!is.na(retirement_start) && retirement_start >= 0) {
          p <- p |>
            plotly::add_segments(
              x = retirement_start,
              xend = retirement_start,
              y = 0,
              yend = max(d$value, na.rm = TRUE),
              inherit = FALSE,
              line = list(dash = "dash", color = "black", width = 1),
              showlegend = FALSE,
              hoverinfo = "none"
            )
        }
      }
      p
    }

    # ---- Compose single vs faceted ----
    if (isTRUE(input$facet_by_group)) {
      df$general_group <- factor(
        df$general_group,
        levels = compartment_group_levels
      )
      split_panels <- split(df, df$general_group, drop = TRUE)
      panels <- Map(
        function(panel_df, i) make_panel(panel_df, show_leg = (i == 1L)),
        split_panels,
        seq_along(split_panels)
      )
      n_pan <- length(panels)
      ncols <- if (n_pan >= 3) 2 else 1
      fig <- plotly::subplot(
        panels,
        nrows = ceiling(n_pan / ncols),
        shareX = TRUE,
        shareY = FALSE,
        titleX = TRUE,
        titleY = TRUE
      ) |>
        plotly::layout(title = list(text = plot_title))
    } else {
      fig <- make_panel(df, show_leg = TRUE) |>
        plotly::layout(title = list(text = plot_title))
    }

    list(
      data = df,
      ggplot = NULL,
      plotly = fig
    )
  })

  output$time.plotly <- plotly::renderPlotly({
    time_plot_components()$plotly
  })

  output$download_time_plot <- shiny::downloadHandler(
    filename = function() {
      paste0("time-point-simulation-", Sys.Date(), ".html")
    },
    content = function(file) {
      plot_widget <- time_plot_components()$plotly
      tmp_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plot_widget, tmp_file, selfcontained = TRUE)
      file.copy(tmp_file, file, overwrite = TRUE)
    }
  )

  output$download_plot_html <- shiny::downloadHandler(
    filename = function() {
      paste0("leggett-time-series-", Sys.Date(), ".html")
    },
    content = function(file) {
      shiny::req(simulation()) # ensure a run exists
      widget <- time_plot_components()$plotly
      tmp <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE)
      file.copy(tmp, file, overwrite = TRUE)
    }
  )

  output$download_time_series <- shiny::downloadHandler(
    filename = function() {
      paste0("time-series-", Sys.Date(), ".csv")
    },
    content = function(file) {
      plot_data <- time_plot_components()$data |>
        dplyr::mutate(
          general_group = as.character(general_group),
          compartment = as.character(compartment)
        ) |>
        dplyr::select(time, general_group, compartment, value) |>
        dplyr::arrange(time, general_group, compartment)
      readr::write_csv(plot_data, file)
    }
  )

  audit_data <- shiny::reactive({
    res_mode <- input$br_mode
    bw <- shiny::req(input$body.weight)
    sim_type <- input$simulation.type

    # Always compute what the solver will see (m3/h → m3/day, plus context)
    # Occupational BR
    occ_m3h <- if (identical(res_mode, "oehha")) {
      work_br_m3h()
    } else {
      input$occ.breath.rate
    }
    # Non-occupational BR
    non_m3h <- if (identical(res_mode, "oehha")) {
      residential_br_m3h()
    } else {
      input$Nonocc.breath.rate
    }

    # Pull the source table values when in OEHHA mode (for transparency)
    res_row <- OEHHA_BR_RES_DAILY_LKGD |>
      dplyr::filter(age_band == input$oehha_res_age) |>
      dplyr::slice_head(n = 1)
    res_Lkgd <- if (identical(input$oehha_res_pct, "p95")) {
      res_row$p95
    } else {
      res_row$mean
    }

    work_row <- OEHHA_BR_WORK_8H_LKG8H |>
      dplyr::filter(intensity == input$oehha_work_intensity) |>
      dplyr::slice_head(n = 1)
    work_Lkg8h <- if (identical(input$oehha_work_pct, "p95")) {
      work_row$p95
    } else {
      work_row$mean
    }

    # WAF context
    waf_used <- isTRUE(input$use_waf)
    wv <- NA_real_
    occ_air_base <- input$occ.airborne.concentration.ug.m3
    occ_air_adj <- occ_air_base
    if (waf_used) {
      wv <- waf_value()
      occ_air_adj <- occ_air_base * wv
    }

    tibble::tibble(
      Field = c(
        "Mode",
        "Body weight (kg)",
        "Residential age band",
        "Residential percentile",
        "Residential BR (Table 5.6, L/kg-day)",
        "Residential BR (converted, m³/h)",
        "Residential BR (converted, m³/day)",
        "Work intensity (Table 5.8)",
        "Work percentile",
        "Work BR (Table 5.8, L/kg-8h)",
        "Work BR (converted, m³/h)",
        "Work BR (converted, m³/day)",
        "Work hours per week (input)",
        "WAF used?",
        "WAF value",
        "Occ air conc (input, µg/m³)",
        "Occ air conc (WAF-adjusted, µg/m³)"
      ),
      Value = c(
        res_mode,
        signif(bw, 6),
        if (identical(res_mode, "oehha")) input$oehha_res_age else "(manual)",
        if (identical(res_mode, "oehha")) input$oehha_res_pct else "(manual)",
        if (identical(res_mode, "oehha")) signif(res_Lkgd, 6) else NA,
        signif(non_m3h, 6),
        signif(non_m3h * 24, 6),
        if (identical(res_mode, "oehha")) {
          input$oehha_work_intensity
        } else {
          "(manual)"
        },
        if (identical(res_mode, "oehha")) input$oehha_work_pct else "(manual)",
        if (identical(res_mode, "oehha")) signif(work_Lkg8h, 6) else NA,
        signif(occ_m3h, 6),
        signif(occ_m3h * 24, 6),
        signif(input$working.hours, 6),
        if (waf_used) "Yes" else "No",
        if (waf_used) signif(wv, 6) else NA,
        signif(occ_air_base, 6),
        signif(occ_air_adj, 6)
      )
    )
  })

  output$br_audit_table <- DT::renderDT({
    df <- audit_data()
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = 't', pageLength = 50),
      class = "stripe hover compact"
    )
  })
}

shiny::shinyApp(ui = ui, server = server)




