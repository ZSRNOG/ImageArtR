.ascii_charsets <- list(
  standard = "@%#*+=-:. ",
  dense = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. ",
  blocks = "\u2588\u2593\u2592\u2591 "
)

#' Create ASCII art from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param width Character width of the output.
#' @param charset Character set: standard, dense, blocks, or custom.
#' @param custom_chars Characters used when `charset = "custom"`.
#' @param invert Reverse brightness-to-character mapping?
#' @param color Use original image colors for plotted or HTML text?
#' @param font_family Font family for ggplot output.
#' @param font_size Font size for ggplot output.
#' @param background Background color.
#' @param text_color Text color when `color = FALSE`.
#' @param output Preferred output representation: text, ggplot, or html.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object. ASCII text and HTML are stored in
#'   `result$processed`.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_ascii_art(path, width = 40, output = "ggplot")
#' plot(result)
image_ascii_art <- function(
  image,
  width = 80,
  charset = c("standard", "dense", "blocks", "custom"),
  custom_chars = NULL,
  invert = FALSE,
  color = FALSE,
  font_family = "mono",
  font_size = 3,
  background = "white",
  text_color = "black",
  output = c("text", "ggplot", "html"),
  max_dimension = 1000
) {
  call <- match.call()
  charset <- .arg_match(charset, c("standard", "dense", "blocks", "custom"), "charset")
  output <- .arg_match(output, c("text", "ggplot", "html"), "output")
  .check_positive_integer(width, "width")
  .check_bool(invert, "invert")
  .check_bool(color, "color")
  .check_number(font_size, "font_size", min = 0.1)
  .check_color(background, "background")
  .check_color(text_color, "text_color")
  .check_max_dimension(max_dimension)

  chars <- .ascii_chars(charset, custom_chars, invert)
  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  info <- .image_info(working)
  char_height <- max(1, round(width * info$height / info$width * 0.5))
  small <- magick::image_resize(working, geometry = sprintf("%sx%s!", width, char_height))
  gray <- .image_luminance(small)
  pixels <- .image_to_pixel_df(small)
  char_matrix <- .ascii_matrix(gray, chars)
  lines <- apply(char_matrix, 1, paste0, collapse = "")
  data <- tibble::tibble(
    x = pixels$x,
    y = pixels$y,
    char = as.vector(t(char_matrix)),
    hex = pixels$hex
  )
  html <- .ascii_html(char_matrix, matrix(pixels$hex, nrow = char_height, byrow = TRUE), color, background, text_color)
  plot <- .ascii_plot(data, width, char_height, color, font_family, font_size, background, text_color)

  .make_image_art(
    original = original,
    processed = list(
      lines = lines,
      text = paste(lines, collapse = "\n"),
      html = html,
      data = data,
      output = output,
      color = color
    ),
    plot = if (identical(output, "text")) NULL else plot,
    image = NULL,
    type = "ascii",
    parameters = list(
      width = width,
      charset = charset,
      invert = invert,
      color = color,
      font_family = font_family,
      font_size = font_size,
      background = background,
      text_color = text_color,
      output = output,
      max_dimension = max_dimension
    ),
    call = call
  )
}

#' Print ASCII art
#'
#' @param x An ASCII `image_art` object, character vector, or text string.
#' @param ... Unused.
#'
#' @return The ASCII lines, invisibly.
#' @export
print_ascii_art <- function(x, ...) {
  lines <- .ascii_lines(x)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(lines)
}

#' Write ASCII art to a text or HTML file
#'
#' @param x An ASCII `image_art` object, character vector, or text string.
#' @param path Output path ending in `.txt` or `.html`.
#' @param overwrite Overwrite an existing file?
#'
#' @return The output path, invisibly.
#' @export
write_ascii_art <- function(x, path, overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    rlang::abort("`path` must be a single output path.")
  }
  .check_bool(overwrite, "overwrite")
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("txt", "html")) {
    rlang::abort("ASCII art can only be written to `.txt` or `.html` files.")
  }
  if (file.exists(path) && !overwrite) {
    rlang::abort(sprintf("Output file already exists: `%s`.", path))
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  content <- if (identical(ext, "html")) .ascii_html_content(x) else paste(.ascii_lines(x), collapse = "\n")
  writeLines(content, con = path, useBytes = TRUE)
  invisible(path)
}

.ascii_chars <- function(charset, custom_chars, invert) {
  chars <- if (identical(charset, "custom")) {
    if (is.null(custom_chars) || !is.character(custom_chars) || length(custom_chars) != 1 || nchar(custom_chars) < 2) {
      rlang::abort("`custom_chars` must be a single string with at least two characters.")
    }
    custom_chars
  } else {
    .ascii_charsets[[charset]]
  }
  split <- strsplit(chars, "", fixed = TRUE)[[1]]
  if (invert) rev(split) else split
}

.ascii_matrix <- function(gray, chars) {
  idx <- pmin(length(chars), pmax(1, floor(as.vector(gray) * (length(chars) - 1)) + 1))
  matrix(chars[idx], nrow = nrow(gray), ncol = ncol(gray))
}

.ascii_plot <- function(data, width, height, color, font_family, font_size, background, text_color) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, label = char))
  if (color) {
    p <- p + ggplot2::geom_text(ggplot2::aes(color = hex), family = font_family, size = font_size) +
      ggplot2::scale_color_identity()
  } else {
    p <- p + ggplot2::geom_text(color = text_color, family = font_family, size = font_size)
  }
  p +
    ggplot2::scale_x_continuous(limits = c(0.5, width + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(0.5, height + 0.5), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 2) +
    .plot_background_theme(background)
}

.ascii_html <- function(char_matrix, color_matrix, color, background, text_color) {
  lines <- vapply(seq_len(nrow(char_matrix)), function(i) {
    chars <- .html_escape(char_matrix[i, ])
    chars[chars == " "] <- "&nbsp;"
    if (color) {
      paste0("<span style=\"color:", color_matrix[i, ], "\">", chars, "</span>", collapse = "")
    } else {
      paste0(chars, collapse = "")
    }
  }, character(1))
  paste0(
    "<!doctype html><html><meta charset=\"utf-8\"><body style=\"margin:0;background:",
    background,
    ";\"><pre style=\"font-family:monospace;color:",
    text_color,
    ";line-height:1;\">",
    paste(lines, collapse = "\n"),
    "</pre></body></html>"
  )
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

.ascii_lines <- function(x) {
  if (inherits(x, "image_art")) {
    if (!identical(x$type, "ascii")) {
      rlang::abort("`x` must be an ASCII image_art object.")
    }
    return(x$processed$lines)
  }
  if (is.character(x) && length(x) > 1) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    return(strsplit(x, "\n", fixed = TRUE)[[1]])
  }
  rlang::abort("`x` must be ASCII image_art, a character vector, or a text string.")
}

.ascii_html_content <- function(x) {
  if (inherits(x, "image_art") && identical(x$type, "ascii")) {
    return(x$processed$html)
  }
  lines <- .ascii_lines(x)
  paste0(
    "<!doctype html><html><meta charset=\"utf-8\"><body><pre>",
    paste(.html_escape(lines), collapse = "\n"),
    "</pre></body></html>"
  )
}
