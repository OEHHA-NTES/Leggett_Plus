test_that("leggett_base_parameters derives blood volumes consistently", {
  params <- leggett_base_parameters(
    body_weight_kg = 73,
    hematocrit = 0.3802,
    age_years = 40,
    initial_bll_ug_dl = 1
  )

  expect_equal(params$blood_volume_dl, 0.7425 * 73)
  expect_equal(
    params$rbc_volume_dl + params$plasma_volume_dl,
    params$blood_volume_dl
  )
  expect_equal(params$rbc_volume_dl, params$blood_volume_dl * 0.3802)
})

test_that("leggett_base_parameters returns the reference RBC saturation constant", {
  params <- leggett_base_parameters(
    body_weight_kg = 73,
    hematocrit = 0.3802,
    age_years = 40,
    initial_bll_ug_dl = 0
  )
  expect_equal(params$ode_parameters$Sat.ug.dL, 270)
})

test_that("leggett_base_parameters scales the RBC saturation constant with inputs", {
  params <- leggett_base_parameters(
    body_weight_kg = 146,
    hematocrit = 0.1901,
    age_years = 40,
    initial_bll_ug_dl = 0
  )
  expect_equal(params$ode_parameters$Sat.ug.dL, 270 * 0.5 * 2)
})

test_that("leggett_base_parameters passes through inhalation_transfer and abs_ratio", {
  params <- leggett_base_parameters(
    body_weight_kg = 73,
    hematocrit = 0.3802,
    age_years = 40,
    initial_bll_ug_dl = 0,
    inhalation_transfer = 0.5,
    abs_ratio = 0.1
  )
  expect_equal(params$ode_parameters$inhalation.transfer, 0.5)
  expect_equal(params$ode_parameters$abs.ratio, 0.1)
})
