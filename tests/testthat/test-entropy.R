test_that("entropy works", {
  .act <- ds_entropy(de_county, starts_with('pop_'))
  .exp <- c(0.638090971561592, 0.638090971561592, 0.638090971561592)
  expect_equal(.act, .exp, tolerance = 1e-6)
})

test_that("entropy .name works", {
  .act <- ds_entropy(de_county, starts_with('pop_'), .name = 'special_name')
  expect_true('special_name' %in% names(.act))
})

test_that("entropy .comp works", {
  .act <- ds_entropy(de_county, starts_with('pop_'), .comp = TRUE)
  .exp <- c(0.116761244988572, 0.399314289368122, 0.122015437204899)
  expect_equal(.act, .exp, tolerance = 1e-6)
})
