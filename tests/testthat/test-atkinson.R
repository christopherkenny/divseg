test_that('atkinson works', {
  .act <- ds_atkinson(de_county, starts_with('pop_'))
  .exp <- c(0.0161486825549041, 0.0161486825549041, 0.0161486825549041)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('atkinson .name works', {
  .act <- ds_atkinson(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
