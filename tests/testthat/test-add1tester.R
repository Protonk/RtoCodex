test_that("add1tester increments integers", {
  expect_equal(add1tester(1L), 2L)
  expect_equal(add1tester(c(1L, 2L, 3L)), c(2L, 3L, 4L))
})
