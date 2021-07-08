test_that("interaction works", {
  .act <- ds_interaction(de_county, starts_with('pop_'))
  .exp <- c(0.341755130327869, 0.341755130327869, 0.341755130327869)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("interaction .name works", {
  .act <- ds_interaction(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("interaction .comp works", {
  .act <- ds_interaction(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.062731341871162, 0.217030538635868, 0.0619932498208391)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

