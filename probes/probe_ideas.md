<!-- probes/probe_ideas.md tracks brainstormed capability probe concepts and notes which ones are queued for later work. -->

# Capability probe ideas

- Locale-sensitive file IO: write and read a UTF-8 string with non-ASCII characters to detect when the default locale or iconv setup strips or mangles multibyte sequences.
- Filesystem case sensitivity: create `CaseProbe.txt` and query for `caseprobe.txt` to differentiate APFS/HFS+ default macOS volumes from Linux ext4.
- Fortran toolchain discovery: inspect `R CMD config FC`/`F77` to see if a Fortran compiler is advertised or silently stubbed out when toolchain packages are missing.
- Temporary directory resolution: compare `tempdir()` with `normalizePath(tempdir())` to detect symlink hops (e.g., `/var/folders` vs `/private/var/...`) that may expose long path issues in build scripts.
- BLAS threading knobs: check whether environment variables like `OPENBLAS_NUM_THREADS` or `MKL_NUM_THREADS` are honored by running a short matrix multiply and inspecting `getOption("Ncpus")`.
- Shared library search order: compile a minimal shared object and inspect `ldd`/`otool -L` output to spot divergent default linkers and runtime paths.
- Timezone resolution: compare `Sys.timezone()` with the `TZ` environment variable to see if `/etc/localtime` symlinks differ across environments.

**Selected for implementation in this run:** locale-sensitive file IO, filesystem case sensitivity, and Fortran toolchain discovery.

**TODO:** Revisit the remaining ideas in future passes once additional environment quirks surface.
