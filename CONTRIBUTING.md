# Contributing

Thanks for helping improve ImageArtR.

## Development workflow

1. Install dependencies with `devtools::install_deps(dependencies = TRUE)`.
2. Run `devtools::document()` after editing roxygen comments.
3. Run `devtools::test()` for the unit tests.
4. Run `devtools::check()` before submitting larger changes.

## Style

Use tidyverse-style R code, clear argument validation, and explicit
`package::function()` calls. Internal helpers should live in `R/utils-*.R` and
use a leading dot in their names.

## Tests

Add focused `testthat` coverage for new behavior. Tests should generate small
images dynamically or use files under `inst/extdata/`; they should not depend on
unstable external image URLs.
