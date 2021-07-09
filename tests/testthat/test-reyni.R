test_that("reyni works", {
  .act <- ds_reyni(de_county, starts_with('pop_'))
  .exp <- c(2.07944154167984, 2.07944154167984, 2.07944154167984)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("reyni .name works", {
  .act <- ds_reyni(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("reyni q works", {
  .act <- ds_reyni(de_county, starts_with('pop_'), q = 0.5)
  .exp <- c(1.37053705449838, 1.38889483614063, 1.25317686892521)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
