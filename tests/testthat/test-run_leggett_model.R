single_stage_schedule <- function(duration = 2) {
  data.frame(
    duration = duration,
    Occ.background.ug.d = 0,
    NonOcc.background.ug.d = 0,
    Occ.pb.ug.m3 = 0,
    NonOcc.pb.ug.m3 = 0,
    Occ.breath.rate.m3.d = 1,
    NonOcc.breath.rate.m3.d = 1,
    work.hour.perweek = 0,
    oral.intake.ug.d = 0
  )
}

test_that("run_leggett_model executes and returns the expected structure", {
  out <- run_leggett_model(single_stage_schedule())
  expect_named(out, c("time_series", "metadata"))
  expect_s3_class(out$time_series, "data.frame")
  expect_true(all(c("time", "plasd", "rbc", "plasb") %in% names(out$time_series)))
})

test_that("run_leggett_model with zero exposure and zero initial BLL stays at zero", {
  out <- run_leggett_model(single_stage_schedule(), initial_bll_ug_dl = 0)
  compartments <- setdiff(names(out$time_series), "time")
  expect_true(all(abs(unlist(out$time_series[compartments])) < 1e-8))
})

test_that("run_leggett_model concatenates multiple stages into one continuous time series", {
  # Each stage's first simulated day coincides with the previous stage's last
  # day, so the combined series has sum(duration) - (n_stages - 1) rows.
  schedule <- rbind(single_stage_schedule(3), single_stage_schedule(4))
  out <- run_leggett_model(schedule)
  expect_identical(out$time_series$time, 0:5)
  expect_identical(nrow(out$time_series), 6L)
})

test_that("run_leggett_model records metadata inputs", {
  out <- run_leggett_model(
    single_stage_schedule(),
    body_weight_kg = 80,
    hematocrit = 0.4,
    age_years = 25,
    initial_bll_ug_dl = 1.5
  )
  expect_equal(out$metadata$body_weight_kg, 80)
  expect_equal(out$metadata$hematocrit, 0.4)
  expect_equal(out$metadata$age_years, 25)
  expect_equal(out$metadata$initial_bll_ug_dl, 1.5)
  expect_equal(out$metadata$blood_volume_dl, 0.7425 * 80)
})

test_that("run_leggett_model propagates schedule validation errors", {
  schedule <- single_stage_schedule()
  schedule$duration <- -1
  expect_error(run_leggett_model(schedule), "must be positive")
})

test_that("oral exposure increases blood lead mass over time", {
  schedule <- single_stage_schedule(30)
  schedule$oral.intake.ug.d <- 10
  out <- run_leggett_model(schedule, initial_bll_ug_dl = 0)
  blood_mass <- out$time_series$plasd + out$time_series$plasb + out$time_series$rbc
  expect_true(blood_mass[nrow(out$time_series)] > blood_mass[1])
})
