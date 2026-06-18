#' Extract a dominant image palette
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param n Number of colors to return.
#' @param colorspace Clustering colorspace.
#' @param sort_by Sorting method.
#' @param remove_transparent Ignore fully transparent pixels?
#'
#' @return A tibble with `hex`, RGB, HSV, frequency, proportion, and colorspace
#'   columns.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' palette <- extract_image_palette(path, n = 5)
#' plot_image_palette(palette)
extract_image_palette <- function(
  image,
  n = 8,
  colorspace = c("RGB", "sRGB", "Lab", "HSL", "HSV"),
  sort_by = c("frequency", "hue", "value", "saturation"),
  remove_transparent = TRUE
) {
  call <- match.call()
  colorspace <- .arg_match(colorspace, c("RGB", "sRGB", "Lab", "HSL", "HSV"), "colorspace")
  sort_by <- .arg_match(sort_by, c("frequency", "hue", "value", "saturation"), "sort_by")
  .check_positive_integer(n, "n")
  .check_bool(remove_transparent, "remove_transparent")
  invisible(call)

  img <- .limit_image_dimension(read_art_image(image), 600)
  pixels <- .image_to_pixel_df(img)
  if (remove_transparent) {
    pixels <- pixels[pixels$alpha > 0, , drop = FALSE]
  }
  if (nrow(pixels) == 0) {
    rlang::abort("No non-transparent pixels are available for palette extraction.")
  }

  features <- .palette_features(pixels, colorspace)
  unique_count <- nrow(unique(as.matrix(pixels[, c("red", "green", "blue")])))
  centers <- min(n, unique_count, nrow(pixels))

  if (centers <= 1) {
    pixels$cluster <- 1L
  } else {
    km <- .with_seed(1L, stats::kmeans(features, centers = centers, iter.max = 40))
    pixels$cluster <- km$cluster
  }

  palette <- dplyr::summarise(
    dplyr::group_by(pixels, cluster),
    red = round(mean(red)),
    green = round(mean(green)),
    blue = round(mean(blue)),
    frequency = dplyr::n(),
    .groups = "drop"
  )
  hsv <- .rgb_to_hsv_table(palette$red, palette$green, palette$blue)
  palette <- tibble::tibble(
    hex = .as_hex(palette$red, palette$green, palette$blue, 255),
    red = as.integer(palette$red),
    green = as.integer(palette$green),
    blue = as.integer(palette$blue),
    hue = hsv$hue,
    saturation = hsv$saturation,
    value = hsv$value,
    frequency = palette$frequency,
    proportion = palette$frequency / sum(palette$frequency),
    colorspace = colorspace
  )

  .sort_palette(palette, sort_by)
}

#' Plot an extracted image palette
#'
#' @param palette A palette data frame returned by `extract_image_palette()`, or
#'   an image input accepted by `extract_image_palette()`.
#' @param ... Passed to `extract_image_palette()` when `palette` is an image.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' plot_image_palette(path, n = 5)
plot_image_palette <- function(palette, ...) {
  if (!is.data.frame(palette) || !"hex" %in% names(palette)) {
    palette <- extract_image_palette(palette, ...)
  }
  palette$label <- sprintf("%s\n%.1f%%", palette$hex, palette$proportion * 100)
  palette$index <- seq_len(nrow(palette))
  ggplot2::ggplot(palette, ggplot2::aes(x = index, y = proportion, fill = hex)) +
    ggplot2::geom_col(width = 0.92, color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      angle = 90,
      hjust = 0.5,
      vjust = 0.5,
      size = 3
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = palette$index, labels = palette$hex) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Frequency") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

.palette_features <- function(pixels, colorspace) {
  rgb <- as.matrix(pixels[, c("red", "green", "blue")]) / 255
  if (identical(colorspace, "Lab")) {
    return(grDevices::convertColor(rgb, from = "sRGB", to = "Lab"))
  }
  if (identical(colorspace, "HSV")) {
    hsv <- .rgb_to_hsv_table(pixels$red, pixels$green, pixels$blue)
    return(as.matrix(hsv))
  }
  if (identical(colorspace, "HSL")) {
    hsl <- .rgb_to_hsl_table(pixels$red, pixels$green, pixels$blue)
    return(as.matrix(hsl))
  }
  rgb
}

.sort_palette <- function(palette, sort_by) {
  if (identical(sort_by, "frequency")) {
    return(dplyr::arrange(palette, dplyr::desc(frequency)))
  }
  if (identical(sort_by, "hue")) {
    return(dplyr::arrange(palette, hue))
  }
  if (identical(sort_by, "value")) {
    return(dplyr::arrange(palette, dplyr::desc(value)))
  }
  dplyr::arrange(palette, dplyr::desc(saturation))
}
