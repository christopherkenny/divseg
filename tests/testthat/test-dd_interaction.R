test_that("dd_interaction works", {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_dd_interaction(de_county, starts_with('pop_'))
  .exp <- c(0.307176832960713, 0.307176832960713, 0.307176832960713)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("dd_interaction .name works", {
  .act <- ds_dd_interaction(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("dd_interaction .comp works", {
  skip_on_os('solaris')
  skip_if_not(sf::sf_use_s2())
  .act <- ds_dd_interaction(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.0554361331858142, 0.173723023598303, 0.0780176761765964)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
