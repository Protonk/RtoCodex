# tests/testthat/test_cap_compilers.R captures how compiler-sensitive tests can
# guard themselves on capability probes emitted by the harness.

test_that("cap_cxx20_flags toggles C++20 dependent work", {
  skip_if(!needs_cap("cap_cxx20_flags", "supported"), "C++20 flags missing")
  expect_true(TRUE)
})

test_that("cap_openmp_flags toggles OpenMP dependent work", {
  skip_if(!needs_cap("cap_openmp_flags", "available"), "OpenMP flags missing")
  expect_true(TRUE)
})
