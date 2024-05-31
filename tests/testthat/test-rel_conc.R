test_that('rel_conc works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_rel_conc(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(0.168090735561022, 0.168090735561022, 0.168090735561022)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('rel_conc .name works', {
  .act <- ds_rel_conc(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
