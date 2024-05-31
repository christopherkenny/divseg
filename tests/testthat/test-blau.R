test_that('blau works', {
  .act <- ds_blau(de_county, starts_with('pop_'))
  .exp <- c(0.515522780376459, 0.557043470752164, 0.405276878378517)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('blau .name works', {
  .act <- ds_blau(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
