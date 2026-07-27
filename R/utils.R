#' Internal helper: trapezoidal area under the curve
#'
#' @param x Numeric vector of x positions.
#' @param y Numeric vector of y values.
#'
#' @return Scalar numeric AUC.
#' @keywords internal
trapz_internal <- function(x, y) {
  if (length(x) < 2) {
    return(0)
  }
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  dx <- diff(x)
  avg_y <- (utils::head(y, -1) + utils::tail(y, -1)) / 2
  sum(dx * avg_y)
}

#' Validate a Leggett+ exposure stage schedule
#'
#' Ensures required columns exist, coerces to numeric, and rounds durations to
#' whole days prior to integration.
#'
#' @param stage_schedule Data frame describing exposure stages.
#'
#' @return Cleaned data frame with numeric columns.
#' @keywords internal
validate_stage_schedule <- function(stage_schedule) {
  required <- c(
    "duration",
    "Occ.background.ug.d",
    "NonOcc.background.ug.d",
    "Occ.pb.ug.m3",
    "NonOcc.pb.ug.m3",
    "Occ.breath.rate.m3.d",
    "NonOcc.breath.rate.m3.d",
    "work.hour.perweek",
    "oral.intake.ug.d"
  )
  missing_cols <- setdiff(required, names(stage_schedule))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Stage schedule is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  stage_schedule <- as.data.frame(stage_schedule)
  stage_schedule[required] <- lapply(
    stage_schedule[required],
    function(col) suppressWarnings(as.numeric(col))
  )

  if (any(is.na(stage_schedule$duration))) {
    stop("All stage durations must be provided.")
  }
  if (any(stage_schedule$duration <= 0)) {
    stop("All stage durations must be positive.")
  }

  replacement_names <- required[required != "duration"]
  stage_schedule <- tidyr::replace_na(
    stage_schedule,
    stats::setNames(
      as.list(rep(0, length(replacement_names))),
      replacement_names
    )
  )
  stage_schedule$duration <- as.integer(round(stage_schedule$duration))
  stage_schedule
}
