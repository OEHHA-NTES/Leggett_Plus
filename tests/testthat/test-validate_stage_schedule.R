base_schedule <- function() {
  data.frame(
    duration = 2,
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

test_that("validate_stage_schedule errors on missing columns", {
  schedule <- base_schedule()
  schedule$duration <- NULL
  expect_error(validate_stage_schedule(schedule), "missing required columns")
})

test_that("validate_stage_schedule errors on non-positive duration", {
  schedule <- base_schedule()
  schedule$duration <- 0
  expect_error(validate_stage_schedule(schedule), "must be positive")
})

test_that("validate_stage_schedule errors on missing duration values", {
  schedule <- base_schedule()
  schedule$duration <- NA
  expect_error(validate_stage_schedule(schedule), "must be provided")
})

test_that("validate_stage_schedule rounds fractional durations to integers", {
  schedule <- base_schedule()
  schedule$duration <- 2.6
  out <- validate_stage_schedule(schedule)
  expect_identical(out$duration, 3L)
})

test_that("validate_stage_schedule replaces NA exposure values with 0", {
  schedule <- base_schedule()
  schedule$oral.intake.ug.d <- NA
  out <- validate_stage_schedule(schedule)
  expect_identical(out$oral.intake.ug.d, 0)
})

test_that("validate_stage_schedule coerces character columns to numeric", {
  schedule <- base_schedule()
  schedule$oral.intake.ug.d <- "5"
  out <- validate_stage_schedule(schedule)
  expect_identical(out$oral.intake.ug.d, 5)
})
