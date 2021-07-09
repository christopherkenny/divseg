test_that("abs_clust works", {
  .act <- ds_abs_clust(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(0.140399280884611, 0.140399280884611, 0.140399280884611)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("abs_clust .name works", {
  .act <- ds_abs_clust(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
