run_reference_model <- function() {
  schedule <- data.frame(
    duration = 10,
    Occ.background.ug.d = 0,
    NonOcc.background.ug.d = 2.2,
    Occ.pb.ug.m3 = 0,
    NonOcc.pb.ug.m3 = 0,
    Occ.breath.rate.m3.d = 0,
    NonOcc.breath.rate.m3.d = 17,
    work.hour.perweek = 0,
    oral.intake.ug.d = 0
  )
  run_leggett_model(schedule, initial_bll_ug_dl = 1)
}

test_that("summarize_leggett_output returns one row per compartment plus derived rows", {
  out <- run_reference_model()
  summary_df <- summarize_leggett_output(
    out$time_series,
    time_point = 5,
    blood_volume_dl = out$metadata$blood_volume_dl,
    plasma_volume_dl = out$metadata$plasma_volume_dl,
    rbc_volume_dl = out$metadata$rbc_volume_dl
  )
  n_compartments <- length(setdiff(names(out$time_series), "time"))
  expect_identical(nrow(summary_df), n_compartments + 3L)
  expect_true("Blood Lead (ug/dL)" %in% summary_df$Compartment)
  expect_true("Red Blood Cell Lead (ug/dL)" %in% summary_df$Compartment)
  expect_true("Plasma Lead (ug/dL)" %in% summary_df$Compartment)
})

test_that("summarize_leggett_output omits derived rows when volumes are not supplied", {
  out <- run_reference_model()
  summary_df <- summarize_leggett_output(
    out$time_series,
    time_point = 5,
    blood_volume_dl = NULL,
    plasma_volume_dl = NULL,
    rbc_volume_dl = NULL
  )
  n_compartments <- length(setdiff(names(out$time_series), "time"))
  expect_identical(nrow(summary_df), n_compartments)
})

test_that("summarize_leggett_output clamps out-of-range time points to the simulated range", {
  out <- run_reference_model()
  early <- summarize_leggett_output(
    out$time_series,
    time_point = -100,
    blood_volume_dl = out$metadata$blood_volume_dl,
    plasma_volume_dl = out$metadata$plasma_volume_dl,
    rbc_volume_dl = out$metadata$rbc_volume_dl
  )
  at_zero <- summarize_leggett_output(
    out$time_series,
    time_point = 0,
    blood_volume_dl = out$metadata$blood_volume_dl,
    plasma_volume_dl = out$metadata$plasma_volume_dl,
    rbc_volume_dl = out$metadata$rbc_volume_dl
  )
  expect_equal(early$Value, at_zero$Value)
})
