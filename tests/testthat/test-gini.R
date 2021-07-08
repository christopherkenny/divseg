test_that("gini works", {
  .act <- ds_gini(de_county, starts_with('pop_'))
  .exp <-c(2.75341018294567e-07, 2.75341018294567e-07, 2.75341018294567e-07)
  expect_equal(.act, .exp, tolerance = 1e-8)
})

test_that("gini .name works", {
  .act <- ds_gini(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("gini .comp works", {
  .act <- ds_gini(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(7.17578265163266e-08, 1.329827757526e-07, 7.06004160256404e-08)
  expect_equal(.act, .exp, tolerance = 1e-8)
})
