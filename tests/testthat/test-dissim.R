test_that('dissim works', {
  .act <- ds_dissim(de_county, starts_with('pop_'))
  .exp <- c(0.099346749779882, 0.099346749779882, 0.099346749779882)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('dissim .name works', {
  .act <- ds_dissim(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that('dissim .comp works', {
  .act <- ds_dissim(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.000417850694234586, 0.0492555241957064, 0.049673374889941)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
