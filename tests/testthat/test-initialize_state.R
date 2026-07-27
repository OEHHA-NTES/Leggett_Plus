test_that("initialize_state returns a length-21 named numeric vector", {
  state <- initialize_state(0.78, blood_volume_dl = 54.2)
  expect_length(state, 21)
  expect_identical(names(state), paste0("A", 1:21))
  expect_type(state, "double")
})

test_that("initialize_state returns all zeros for zero initial BLL", {
  state <- initialize_state(0, blood_volume_dl = 54.2)
  expect_true(all(state == 0))
})

test_that("initialize_state scales linearly with initial BLL", {
  state1 <- initialize_state(1, blood_volume_dl = 54.2)
  state2 <- initialize_state(2, blood_volume_dl = 54.2)
  expect_equal(state2, state1 * 2)
})
