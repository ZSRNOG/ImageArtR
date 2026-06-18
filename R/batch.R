#' Batch process images
#'
#' @param images A character vector of image paths, or a single directory path.
#' @param output_dir Directory for generated files.
#' @param type Transformation type passed to `image_artify()`.
#' @param recursive Recursively read image files when `images` is a directory?
#' @param pattern File pattern used when `images` is a directory.
#' @param format Output format, usually png, jpg, tiff, pdf, or svg.
#' @param overwrite Overwrite existing files?
#' @param parallel Reserved for a future parallel implementation. Must be
#'   `FALSE` in this version.
#' @param ... Arguments passed to `image_artify()`.
#'
#' @return A tibble log with input, output, success, elapsed time, and error.
#' @export
#'
#' @examples
#' \dontrun{
#' batch_image_artify("images", "out", type = "mosaic", tile_size = 12)
#' }
batch_image_artify <- function(
  images,
  output_dir,
  type = c(
    "circle", "mosaic", "outline", "sketch", "stipple", "halftone",
    "hex_mosaic", "pop_art", "duotone", "ascii"
  ),
  recursive = FALSE,
  pattern = "\\.(png|jpe?g|tiff?|bmp|gif)$",
  format = "png",
  overwrite = FALSE,
  parallel = FALSE,
  ...
) {
  type <- .arg_match(type, .art_style_choices(), "type")
  .check_bool(recursive, "recursive")
  .check_bool(overwrite, "overwrite")
  .check_bool(parallel, "parallel")
  if (parallel) {
    rlang::abort("`parallel = TRUE` is reserved for a future version.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
    rlang::abort("`output_dir` must be a single directory path.")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  paths <- .resolve_batch_inputs(images, recursive = recursive, pattern = pattern)
  purrr::map_dfr(paths, function(path) {
    start <- proc.time()[["elapsed"]]
    output <- .batch_output_path(path, output_dir, type, format)
    result <- tryCatch(
      {
        if (file.exists(output) && !overwrite) {
          rlang::abort(sprintf("Output already exists: `%s`.", output))
        }
        art <- image_artify(path, type = type, ...)
        save_image_art(art, output)
        list(success = TRUE, error = NA_character_)
      },
      error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      }
    )
    elapsed <- proc.time()[["elapsed"]] - start
    tibble::tibble(
      input = path,
      output = output,
      type = type,
      success = result$success,
      elapsed = elapsed,
      error = result$error
    )
  })
}

.resolve_batch_inputs <- function(images, recursive, pattern) {
  if (!is.character(images) || length(images) < 1 || any(is.na(images))) {
    rlang::abort("`images` must be a character vector of paths or one directory path.")
  }
  if (length(images) == 1 && dir.exists(images)) {
    paths <- list.files(images, pattern = pattern, full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  } else {
    paths <- images
  }
  normalizePath(paths, winslash = "/", mustWork = FALSE)
}

.batch_output_path <- function(path, output_dir, type, format) {
  stem <- tools::file_path_sans_ext(basename(path))
  file.path(output_dir, sprintf("%s_%s.%s", stem, type, format))
}
