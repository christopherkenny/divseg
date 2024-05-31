test_that('entropy works', {
  .act <- ds_entropy(de_county, starts_with('pop_'))
  .exp <- c(0.0111514471973371, 0.0111514471973371, 0.0111514471973371)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('entropy .name works', {
  .act <- ds_entropy(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that('entropy .comp works', {
  .act <- ds_entropy(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(-0.000185325986678492, -0.0191300983383268, 0.0304668715223424)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
