test_that("rel_clust works", {
  .act <- ds_rel_clust(de_county, c(pop_black, starts_with('pop_')))
  .exp <- c(3.15657598881599, 3.15657598881599, 3.15657598881599)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("rel_clust .name works", {
  .act <- ds_rel_clust(de_county, c(pop_black, starts_with('pop_')), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})
