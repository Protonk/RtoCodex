# tests/unit/test_capabilities.R documents how capability probes gate work that
# depends on host toolchains or sandbox permissions.

test_case("cap_cxx20_flags toggles C++20 dependent work", {
  skip_if(!RtoCodex:::needs_cap("cap_cxx20_flags", "supported"), "C++20 flags missing")
  assert_true(TRUE)
})

test_case("cap_openmp_flags toggles OpenMP dependent work", {
  skip_if(!RtoCodex:::needs_cap("cap_openmp_flags", "available"), "OpenMP flags missing")
  assert_true(TRUE)
})

test_case("cap_sysctl_kern_boottime gates sysctl-dependent tests", {
  skip_if(
    !RtoCodex:::needs_cap("cap_sysctl_kern_boottime", "supported"),
    "sysctl kern.boottime not supported in this environment"
  )
  assert_true(TRUE)
})

test_case("caps helper returns NA when capability artifacts are missing", {
  cap_name <- sprintf("cap_unit_missing_%s", as.integer(runif(1, 1, 100000)))
  assert_true(is.na(RtoCodex:::read_cap(cap_name)))
  assert_false(RtoCodex:::needs_cap(cap_name, "supported"))
})

test_case("caps helper reads newly created capability files", {
  cap_dir <- RtoCodex:::caps_dir()
  dir.create(cap_dir, recursive = TRUE, showWarnings = FALSE)
  cap_name <- sprintf("cap_unit_present_%s", Sys.getpid())
  cap_path <- file.path(cap_dir, sprintf("%s.txt", cap_name))
  on.exit(unlink(cap_path, recursive = FALSE, force = TRUE), add = TRUE)
  writeLines("supported", cap_path)
  assert_equal(RtoCodex:::read_cap(cap_name), "supported")
  assert_true(RtoCodex:::needs_cap(cap_name, "supported"))
})
