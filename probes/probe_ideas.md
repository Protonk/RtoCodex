<!-- probes/probe_ideas.md captures capability coverage notes plus probe concepts that still need implementation. -->
# Probe Ideation Notes

## Coverage Map
- **Heavily probed:** toolchains (C++20/OpenMP flags, Fortran SHLIB, pkg-config), filesystem basics (case sensitivity, long tmp paths, executable symlinks), and UTF-8 string I/O already have multiple probes plus unit coverage.
- **Lightly probed:** resource limits (ulimits, CPU quotas), numerical backends (BLAS/LAPACK selection, threading), locale-side filesystem quirks (Unicode filenames, normalization, decomposed characters), IPC primitives (UNIX sockets/FIFOs), and process controls (fork availability, TMPDIR permissions) currently have little or no coverage.

## Candidate Probe Backlog
| Probe | Subsystem | Signal & Differentiator | Priority | Status |
| --- | --- | --- | --- | --- |
| cap_unicode_filenames | Filesystem & encodings | Attempt to create/list a UTF-8 filename that mixes NFC/NFD characters to see whether macOS normalizes paths (should report `nfd_normalized`) while Linux preserves NFC; also surfaces locales that cannot create such names at all. | High | **Selected this run** (implemented) |
| cap_open_files_limit | Resource limits | Runs `ulimit -n` via POSIX shell and buckets the numeric limit (low/medium/high/unlimited) so the harness can predict when tests opening many files will fail differently between macOS (often low) and containers (often high). | High | **Selected this run** (implemented) |
| cap_blas_backend | Numerical/toolchain | Reads `extSoftVersion()['BLAS']` to classify Accelerate vs OpenBLAS vs MKL vs reference BLAS, which often diverges between macOS installations and Ubuntu containers and affects floating-point/parallel behavior. | High | **Selected this run** (implemented) |
| cap_parallel_fork_mclapply | Process/forking | Calls `parallel::mclapply()` with `mc.cores = 2` to detect environments that disable forked workers (e.g., sandboxed macOS shells) vs Linux containers that allow them; warns harnesses before they request multi-core tests. | Medium | Backlog |
| cap_temp_fifo_support | Filesystem / IPC | Creates a FIFO with `fifo()` in `tempdir()` and checks if writing through it succeeds, flagging filesystems that forbid FIFOs (common on some mounted macOS volumes) versus tmpfs-backed Linux containers. | Medium | Backlog |
| cap_locale_collation | Locale | Sorts accented strings under the default locale to see whether collation honors locale rules (e.g., macOS en_US vs container C.UTF-8) which impacts deterministic ordering tests. | Medium | Backlog |
| cap_umask_tmp_permissions | Filesystem / permissions | Spawns a shell to create files with known `umask` values and inspects resulting mode bits, revealing sandboxes that clamp permissions differently (notably macOS SIP) compared to containers. | Low | Backlog |
| cap_tmp_socket_length | IPC | Uses a small helper to create UNIX domain sockets with progressively longer paths until hitting `sun_path` limits; macOS (shorter limit) vs Linux (longer) influences where harnesses may create socket files. | Medium | Backlog |
| cap_linker_rpath_flags | Toolchain/linker | Runs `R CMD SHLIB` on a stub that injects `-Wl,-rpath` to confirm whether linkers accept custom run-path hints (GNU ld vs Apple ld behave differently), so future C probes know when to expect warnings. | Medium | Backlog |
| cap_space_dir_exec | Filesystem/execution | Creates a directory with embedded spaces + parentheses, copies a helper script there, and executes it to detect quoting issues in shells (macOS default zsh) vs container bash. | Low | Backlog |
| cap_temp_fifo_manyfiles | Filesystem stress | Opens hundreds of zero-byte files in a temp subtree to see whether quotas or inode limits trigger errors (more common inside small tmpfs mounts) before fuzz harnesses spam tiny artifacts. | Low | Backlog |

Implemented probes are wired into the harness below; remaining concepts stay here as a prioritized backlog for future runs.
