test_that("prepare_tree_data works", {
  expect_no_error(tree_data())
})

test_that("prepare_tree_data works", {
  expect_no_error(tree_data(collapse=NULL))
})

test_that("prepare_tree_data works", {
  expect_no_error(tree_data(collapse='default'))
})

test_that("prepare_tree_data works", {
  expect_no_error(tree_data(collapse='none'))
})
