test_that("diversity works", {
  .act <- ds_diversity(de_county, starts_with('pop_'))
  .exp <- c(1, 1, 1)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("diversity .name works", {
  .act <- ds_diversity(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("diversity q works", {
  .act <- ds_diversity(de_county, starts_with('pop_'), q = 5)
  .exp <- c(1.70305496726665, 1.82809403787865, 1.41870817022458)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
