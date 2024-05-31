test_that('dd_isolation works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_dd_isolation(de_county, starts_with('pop_'))
  .exp <- c(0.692823167039287, 0.692823167039287, 0.692823167039287)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('dd_isolation .name works', {
  .act <- ds_dd_isolation(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that('dd_isolation .comp works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_dd_isolation(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.12503363939272, 0.391824260433111, 0.175965267213456)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
