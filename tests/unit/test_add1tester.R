# tests/unit/test_add1tester.R verifies the compiled add1tester() wrapper so we
# notice regressions in the .C interface during refactors.

test_case("add1tester increments integers", {
  assert_equal(RtoCodex::add1tester(1L), 2L)
  assert_equal(RtoCodex::add1tester(c(1L, 2L, 3L)), c(2L, 3L, 4L))
})

test_case("add1tester handles doubles and empty vectors", {
  assert_equal(RtoCodex::add1tester(c(1.2, 4.8)), c(2L, 5L))
  assert_equal(RtoCodex::add1tester(integer()), integer())
})
