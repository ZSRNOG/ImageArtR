.new_image_art <- function(
  original,
  processed,
  plot = NULL,
  image = NULL,
  type,
  parameters,
  call
) {
  out <- list(
    original = original,
    processed = processed,
    plot = plot,
    image = image,
    type = type,
    parameters = parameters,
    call = call
  )
  class(out) <- "image_art"
  out
}

#' Print an image art object
#'
#' @param x An `image_art` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.image_art <- function(x, ...) {
  info <- tryCatch(.image_info(x$original), error = function(e) NULL)
  size <- if (is.null(info)) {
    "unknown"
  } else {
    sprintf("%s x %s", info$width, info$height)
  }
  cat("<image_art>\n")
  cat("Type: ", x$type, "\n", sep = "")
  cat("Original size: ", size, "\n", sep = "")
  if (length(x$parameters) > 0) {
    shown <- utils::head(names(x$parameters), 6)
    values <- vapply(shown, function(name) {
      value <- x$parameters[[name]]
      paste(utils::head(as.character(value), 3), collapse = ", ")
    }, character(1))
    cat("Parameters: ", paste(sprintf("%s=%s", shown, values), collapse = "; "), "\n", sep = "")
  }
  invisible(x)
}

#' Plot an image art object
#'
#' @param x An `image_art` object.
#' @param ... Unused.
#'
#' @return A `ggplot2` object, invisibly.
#' @export
plot.image_art <- function(x, ...) {
  if (!is.null(x$plot)) {
    p <- x$plot
  } else if (!is.null(x$image)) {
    p <- .magick_to_ggplot(x$image)
  } else {
    rlang::abort("This `image_art` object does not contain a plot or image.")
  }
  print(p)
  invisible(p)
}

#' Summarize an image art object
#'
#' @param object An `image_art` object.
#' @param ... Unused.
#'
#' @return A one-row tibble with type, original dimensions, output kind, and
#'   parameters.
#' @export
summary.image_art <- function(object, ...) {
  info <- tryCatch(.image_info(object$original), error = function(e) NULL)
  tibble::tibble(
    type = object$type,
    original_width = if (is.null(info)) NA_integer_ else info$width,
    original_height = if (is.null(info)) NA_integer_ else info$height,
    has_plot = !is.null(object$plot),
    has_image = !is.null(object$image),
    parameters = list(object$parameters)
  )
}
