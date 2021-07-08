test_that("correlation works", {
  .act <- ds_correlation(de_county, starts_with('pop_'))
  .exp <- c(0.0138454306617205, 0.0138454306617205, 0.0138454306617205)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("correlation .name works", {
  .act <- ds_correlation(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

