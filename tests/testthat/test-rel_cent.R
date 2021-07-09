test_that("rel_cent works", {
  .act <- ds_rel_cent(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(0.112546751234157, 0.112546751234157, 0.112546751234157)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("rel_cent .name works", {
  .act <- ds_rel_cent(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
