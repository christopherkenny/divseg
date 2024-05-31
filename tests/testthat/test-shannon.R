test_that('shannon works', {
  .act <- ds_shannon(de_county, starts_with('pop_'))
  .exp <- c(1.00877411703218, 1.08342378299773, 0.834855336550792)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that('shannon .name works', {
  .act <- ds_shannon(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
