# tests/testthat/test_cap_sysctl_kern_boottime.R shows how tests consume
# capability probes so sandbox quirks become named, replayable signals.

test_that("cap_sysctl_kern_boottime gates sysctl-dependent tests", {
  skip_if(
    !needs_cap("cap_sysctl_kern_boottime", "supported"),
    "sysctl kern.boottime not supported in this environment"
  )
  # Real tests would exercise ps::ps_boot_time() assumptions here.
  expect_true(TRUE)
})
