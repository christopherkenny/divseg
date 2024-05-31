test_that('inv_simpson works', {
  .act <- ds_inv_simpson(de_county, starts_with('pop_'))
  .exp <- c(2.06408053773311, 2.25755787299953, 1.68145472009487)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('inv_simpson .name works', {
  .act <- ds_inv_simpson(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
