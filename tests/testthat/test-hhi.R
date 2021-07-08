test_that("hhi works", {
  .act <- ds_hhi(de_county, starts_with('pop_'))
  .exp <- c(0.484477219623541, 0.442956529247836, 0.594723121621483)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("hhi .name works", {
  .act <- ds_hhi(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
