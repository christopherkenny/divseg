test_that('delta works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_delta(de_county, starts_with('pop_'))
  .exp <- c(0.344690631688565, 0.344690631688565, 0.344690631688565)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('delta .name works', {
  .act <- ds_delta(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that('delta .comp works', {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_delta(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.0576058775521049, 0.172345315844282, 0.114739438292178)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
