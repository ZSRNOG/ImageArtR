## Test environments

* Windows 11 x64, R 4.5.2.

## R CMD check results

`devtools::test()` completed successfully:

* 124 passing tests

`devtools::check()` completed on a clean source directory that excludes Codex's
deep `.git/refs/codex` worktree metadata. The remote CRAN incoming check was
disabled to avoid network-only CRAN `archive.rds` timeouts:

* 0 errors
* 0 warnings
* 1 note

The note was environment-related:

* checking for future file timestamps ... NOTE
  unable to verify current time

## Notes

This is a new package submission.
