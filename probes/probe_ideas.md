<!--
probes/probe_ideas.md collects small, environment-sensitive capability probe
concepts so agents can pick them up without rediscovering the rationale.
-->

# Probe ideas for future capability checks

- **Selected – cap_fortran_shlib**: Compile a trivial Fortran subroutine via `R CMD SHLIB` and classify whether the host toolchain provides `gfortran` or equivalent; macOS ships Fortran inconsistently while the universal container installs it explicitly, so this surfaces missing runtime linkers quickly.
- **Selected – cap_utf8_locale**: Use `iconv()` plus a temporary file round-trip containing escaped Unicode codepoints to check whether the current locale can encode/decode multibyte UTF-8 strings; headless Linux containers often default to `C` locale while laptops usually default to UTF-8, which changes parser behavior.
- **Selected – cap_temp_symlink_exec**: Build a short shell script inside `tempdir()`, symlink it into a nested path, and execute it; some corporate macOS machines mount temporary directories with `noexec` or restrict symlink creation inside `/var/folders`.
- **Selected – cap_pkg_config_path**: Detect whether `pkg-config` exists on `PATH` and can resolve a stub `.pc` file, since Linux containers tend to bundle it while stock macOS lacks it without Homebrew, which impacts optional system dependencies.
- **Selected – cap_long_tmp_paths**: Create a >200-character nested directory under `tempdir()` and check file creation succeeds, because long sandbox prefixes like `/var/folders/...` can break poorly written build scripts.
- TODO – cap_blas_thread_env: Read and toggle BLAS/OpenBLAS thread-env vars (e.g., `OPENBLAS_NUM_THREADS=1`) before a small matrix multiply to see whether the runtime honors the override; this often differs between Accelerate on macOS and OpenBLAS on Linux.
- **Selected – cap_case_sensitive_tmpfs**: Drop a file whose casing differs only by letters and observe whether the filesystem treats them as unique entries; macOS default HFS/APFS installations are case-insensitive whereas Linux tmpfs/ext4 are case-sensitive, so this affects tests that assume unique file identifiers.
