test_that('spat_prox works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_spat_prox(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(6.38904911961338, 6.38904911961338, 6.38904911961338)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('spat_prox .name works', {
  .act <- ds_spat_prox(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
