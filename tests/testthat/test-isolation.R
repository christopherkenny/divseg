test_that('isolation works', {
  .act <- ds_isolation(de_county, starts_with('pop_'))
  .exp <- c(0.658244869672131, 0.658244869672131, 0.658244869672131)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('isolation .name works', {
  .act <- ds_isolation(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that('isolation .comp works', {
  .act <- ds_isolation(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.117738430707372, 0.348516745395546, 0.191989693569213)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
