#' Run the Leggett+ lead PBPK model
#'
#' Integrates a user-provided exposure schedule through the Leggett+ system of
#' differential equations.
#'
#' @section Male-reference physiology:
#' The default physiological parameters (`body_weight_kg`, `hematocrit`) and
#' the underlying calibration constants are derived from adult male /
#' male-dominated occupational-worker data (Leggett, 1993, and subsequent
#' updates). There is no female-specific parameter set (e.g. a lower
#' hematocrit range or pregnancy-related blood-volume expansion); results
#' should not be assumed to represent female physiology.
#'
#' @param stage_schedule Data frame of exposure stages with required columns:
#'   `duration`, `Occ.background.ug.d`, `NonOcc.background.ug.d`,
#'   `Occ.pb.ug.m3`, `NonOcc.pb.ug.m3`, `Occ.breath.rate.m3.d`,
#'   `NonOcc.breath.rate.m3.d`, `work.hour.perweek`, and `oral.intake.ug.d`.
#' @param body_weight_kg Body weight (kg). Model defaults and calibration
#'   constants are referenced to adult male physiology.
#' @param hematocrit Fractional hematocrit (0-1). Model defaults and
#'   calibration constants are referenced to adult male physiology; no
#'   female-specific range is implemented.
#' @param age_years Age (years).
#' @param initial_bll_ug_dl Initial blood lead level (micrograms per deciliter).
#' @param inhalation_transfer Fraction of inhaled lead transferred to plasma.
#' @param abs_ratio Oral absorption ratio.
#'
#' @return List with `time_series` (data frame of compartment masses) and
#'   `metadata` describing the run.
#' @export
#' @examples
#' schedule <- data.frame(
#'   duration = 2,
#'   Occ.background.ug.d = 0,
#'   NonOcc.background.ug.d = 0,
#'   Occ.pb.ug.m3 = 0,
#'   NonOcc.pb.ug.m3 = 0,
#'   Occ.breath.rate.m3.d = 1,
#'   NonOcc.breath.rate.m3.d = 1,
#'   work.hour.perweek = 0,
#'   oral.intake.ug.d = 0
#' )
#' result <- run_leggett_model(schedule)
#' names(result)
run_leggett_model <- function(
  stage_schedule,
  body_weight_kg = 73,
  hematocrit = 0.3802,
  age_years = 62.45,
  initial_bll_ug_dl = 0,
  inhalation_transfer = 0.30,
  abs_ratio = 0.2
) {
  stage_schedule <- validate_stage_schedule(stage_schedule)
  setup <- leggett_base_parameters(
    body_weight_kg = body_weight_kg,
    hematocrit = hematocrit,
    age_years = age_years,
    initial_bll_ug_dl = initial_bll_ug_dl,
    inhalation_transfer = inhalation_transfer,
    abs_ratio = abs_ratio
  )

  ode_fun <- leggett_ode(setup$ode_parameters)
  state <- setup$initial_state

  outfinal <- NULL
  for (i in seq_len(nrow(stage_schedule))) {
    stage <- stage_schedule[i, ]
    times <- seq(0, stage$duration - 1)
    if (length(times) < 2) {
      times <- c(0, max(stage$duration, 1))
    }
    stage_params <- as.list(stage)
    stage_params$duration <- NULL

    result <- deSolve::ode(
      y = state,
      times = times,
      func = ode_fun,
      parms = stage_params
    )
    result_df <- as.data.frame(result)
    if (is.null(outfinal)) {
      outfinal <- result_df
    } else {
      outfinal <- dplyr::bind_rows(outfinal, result_df[-1, ])
    }
    state <- result[nrow(result), -1]
  }

  outfinal$time <- seq(0, nrow(outfinal) - 1)
  names(outfinal) <- c(
    "time",
    "plasd",
    "evf",
    "rbc",
    "plasb",
    "csf",
    "cex",
    "cne",
    "tsf",
    "tex",
    "tne",
    "livhu",
    "livlu",
    "strpd",
    "stmid",
    "stslo",
    "brain",
    "kidoth",
    "sint",
    "kidurp",
    "lintu",
    "lintl"
  )

  list(
    time_series = outfinal,
    metadata = list(
      stage_schedule = stage_schedule,
      body_weight_kg = body_weight_kg,
      hematocrit = hematocrit,
      age_years = age_years,
      initial_bll_ug_dl = initial_bll_ug_dl,
      blood_volume_dl = setup$blood_volume_dl,
      plasma_volume_dl = setup$plasma_volume_dl,
      rbc_volume_dl = setup$rbc_volume_dl
    )
  )
}
