test_that("abs_conc works", {
  .act <- ds_abs_conc(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(0.810913866539401, 0.810913866539401, 0.810913866539401)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("abs_conc .name works", {
  .act <- ds_abs_conc(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
