test_that("add1 increments integers", {
  expect_equal(add1(1L), 2L)
  expect_equal(add1(c(1L, 2L, 3L)), c(2L, 3L, 4L))
})
