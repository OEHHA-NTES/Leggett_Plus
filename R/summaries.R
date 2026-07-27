#' Summarize Leggett+ simulation output
#'
#' Generates compartment-level summaries (value at a target time, maxima, and
#' area under the curve) from a time series produced by [run_leggett_model()].
#'
#' @param time_series Data frame returned in the `time_series` element of
#'   [run_leggett_model()].
#' @param time_point Time (days) at which to extract compartment values.
#' @param blood_volume_dl Total blood volume (deciliters); used for derived
#'   BLL.
#' @param plasma_volume_dl Plasma volume (deciliters); used for derived
#'   plasma concentrations.
#' @param rbc_volume_dl Red blood cell volume (deciliters). Included for
#'   completeness.
#'
#' @return Data frame of summary statistics per compartment.
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
#' res <- run_leggett_model(schedule)
#' summarize_leggett_output(
#'   res$time_series,
#'   time_point = 1,
#'   blood_volume_dl = res$metadata$blood_volume_dl,
#'   plasma_volume_dl = res$metadata$plasma_volume_dl,
#'   rbc_volume_dl = res$metadata$rbc_volume_dl
#' )
summarize_leggett_output <- function(
  time_series,
  time_point,
  blood_volume_dl,
  plasma_volume_dl,
  rbc_volume_dl
) {
  compartments <- setdiff(names(time_series), "time")
  target_time <- max(
    min(time_point, max(time_series$time)),
    min(time_series$time)
  )

  summary_rows <- lapply(compartments, function(compartment) {
    values <- time_series[[compartment]]
    approx_value <- stats::approx(
      time_series$time,
      values,
      xout = target_time,
      rule = 2
    )$y
    max_idx <- which.max(values)
    data.frame(
      Compartment = compartment,
      Value = approx_value,
      Max = values[max_idx],
      Time_of_Max_days = time_series$time[max_idx],
      AUC = trapz_internal(time_series$time, values),
      stringsAsFactors = FALSE
    )
  })
  summary_df <- dplyr::bind_rows(summary_rows)

  # All 21 state-variable compartments track lead mass in micrograms (ug);
  # label them accordingly before appending the derived concentration rows
  # below.
  summary_df$Compartment <- paste0(
    dplyr::recode(
      summary_df$Compartment,
      !!!LEGGETT_COMPARTMENT_LABELS,
      .default = summary_df$Compartment
    ),
    " (ug)"
  )

  if (!is.null(blood_volume_dl) && blood_volume_dl > 0) {
    blood_mass <- time_series$rbc + time_series$plasd + time_series$plasb
    blood_conc <- blood_mass / blood_volume_dl
    bll_value <- stats::approx(
      time_series$time,
      blood_conc,
      xout = target_time,
      rule = 2
    )$y
    bll_max_idx <- which.max(blood_conc)
    summary_df <- dplyr::bind_rows(
      summary_df,
      data.frame(
        Compartment = "Blood Lead (ug/dL)",
        Value = bll_value,
        Max = blood_conc[bll_max_idx],
        Time_of_Max_days = time_series$time[bll_max_idx],
        AUC = trapz_internal(time_series$time, blood_conc),
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(rbc_volume_dl) && rbc_volume_dl > 0) {
    rbc_conc <- time_series$rbc / rbc_volume_dl
    rbc_value <- stats::approx(
      time_series$time,
      rbc_conc,
      xout = target_time,
      rule = 2
    )$y
    rbc_max_idx <- which.max(rbc_conc)
    summary_df <- dplyr::bind_rows(
      summary_df,
      data.frame(
        Compartment = "Red Blood Cell Lead (ug/dL)",
        Value = rbc_value,
        Max = rbc_conc[rbc_max_idx],
        Time_of_Max_days = time_series$time[rbc_max_idx],
        AUC = trapz_internal(time_series$time, rbc_conc),
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(plasma_volume_dl) && plasma_volume_dl > 0) {
    plasma_mass <- time_series$plasd + time_series$plasb
    plasma_conc <- plasma_mass / plasma_volume_dl
    plasma_value <- stats::approx(
      time_series$time,
      plasma_conc,
      xout = target_time,
      rule = 2
    )$y
    plasma_max_idx <- which.max(plasma_conc)
    summary_df <- dplyr::bind_rows(
      summary_df,
      data.frame(
        Compartment = "Plasma Lead (ug/dL)",
        Value = plasma_value,
        Max = plasma_conc[plasma_max_idx],
        Time_of_Max_days = time_series$time[plasma_max_idx],
        AUC = trapz_internal(time_series$time, plasma_conc),
        stringsAsFactors = FALSE
      )
    )
  }

  tidyr::drop_na(summary_df)
}
