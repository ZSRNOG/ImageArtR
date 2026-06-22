#' Create a style comparison grid
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param styles Character vector of styles to render. Use `"original"` for the
#'   source image, plus any style from `list_art_styles()`.
#' @param options Named list of per-style argument lists. For example,
#'   `list(halftone = list(cell_size = 6), stipple = list(n = 3000))`.
#' @param labels Optional labels shown under each tile.
#' @param ncol Number of columns. Defaults to a near-square layout.
#' @param tile_width Tile width in pixels before spacing.
#' @param tile_height Tile height in pixels before spacing.
#' @param label_height Label strip height in pixels.
#' @param label_size Label text size.
#' @param gap Space around each tile in pixels.
#' @param background Background color for the grid.
#' @param label_color Label text color.
#' @param max_dimension Maximum source image dimension used for preview renders.
#' @param dpi Plot rendering resolution used for ggplot-based styles.
#'
#' @return An `image_art` object containing a `magick` image and a preview plot.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' grid <- image_style_grid(path, styles = c("original", "mosaic", "halftone"))
#' plot(grid)
image_style_grid <- function(
  image,
  styles = c("original", "mosaic", "halftone", "duotone", "pop_art", "ascii"),
  options = list(),
  labels = NULL,
  ncol = NULL,
  tile_width = 360,
  tile_height = 270,
  label_height = 34,
  label_size = 16,
  gap = 10,
  background = "white",
  label_color = "black",
  max_dimension = 600,
  dpi = 150
) {
  call <- match.call()
  .check_style_grid_styles(styles)
  options <- .check_style_grid_options(options)
  .check_number(ncol, "ncol", min = 1, integer = TRUE, allow_null = TRUE)
  .check_number(tile_width, "tile_width", min = 80, integer = TRUE)
  .check_number(tile_height, "tile_height", min = 80, integer = TRUE)
  .check_number(label_height, "label_height", min = 0, integer = TRUE)
  .check_number(label_size, "label_size", min = 1)
  .check_number(gap, "gap", min = 0, integer = TRUE)
  .check_color(background, "background")
  .check_color(label_color, "label_color")
  .check_max_dimension(max_dimension)
  .check_number(dpi, "dpi", min = 1)

  if (tile_height <= label_height) {
    rlang::abort("`tile_height` must be greater than `label_height`.")
  }

  labels <- .style_grid_labels(styles, labels)
  ncol <- ncol %||% ceiling(sqrt(length(styles)))
  source <- .limit_image_dimension(read_art_image(image), max_dimension)
  body_height <- tile_height - label_height

  arts <- purrr::map(styles, function(style) {
    .style_grid_render_style(source, style, options, max_dimension)
  })
  tiles <- purrr::map2(arts, labels, function(art, label) {
    .style_grid_tile(
      art = art,
      label = label,
      tile_width = tile_width,
      body_height = body_height,
      label_height = label_height,
      label_size = label_size,
      gap = gap,
      background = background,
      label_color = label_color,
      dpi = dpi
    )
  })
  image_out <- .style_grid_image(tiles, ncol = ncol, background = background)

  processed <- tibble::tibble(
    style = styles,
    label = labels,
    art = arts
  )

  .make_image_art(
    original = source,
    processed = processed,
    plot = .magick_to_ggplot(image_out, background = background),
    image = image_out,
    type = "style_grid",
    parameters = list(
      styles = styles,
      options = options,
      labels = labels,
      ncol = ncol,
      tile_width = tile_width,
      tile_height = tile_height,
      label_height = label_height,
      label_size = label_size,
      gap = gap,
      background = background,
      label_color = label_color,
      max_dimension = max_dimension,
      dpi = dpi
    ),
    call = call
  )
}

.check_style_grid_styles <- function(styles) {
  choices <- c("original", .art_style_choices())
  if (!is.character(styles) || length(styles) < 1 || any(is.na(styles))) {
    rlang::abort("`styles` must be a character vector of art styles.")
  }
  bad <- setdiff(styles, choices)
  if (length(bad) > 0) {
    rlang::abort(sprintf(
      "`styles` must use values from: %s.",
      paste(choices, collapse = ", ")
    ))
  }
  invisible(styles)
}

.check_style_grid_options <- function(options) {
  if (!is.list(options)) {
    rlang::abort("`options` must be a named list of per-style argument lists.")
  }
  if (length(options) == 0) {
    return(options)
  }
  option_names <- names(options)
  if (is.null(option_names) || any(option_names == "")) {
    rlang::abort("`options` must be a named list.")
  }
  bad_names <- setdiff(option_names, .art_style_choices())
  if (length(bad_names) > 0) {
    rlang::abort(sprintf("Unknown style in `options`: %s.", paste(bad_names, collapse = ", ")))
  }
  bad_values <- !vapply(options, is.list, logical(1))
  if (any(bad_values)) {
    rlang::abort("Each entry in `options` must be a list of arguments.")
  }
  options
}

.style_grid_labels <- function(styles, labels) {
  if (is.null(labels)) {
    out <- tools::toTitleCase(gsub("_", " ", styles, fixed = TRUE))
    out[styles == "ascii"] <- "ASCII"
    return(out)
  }
  if (!is.character(labels) || length(labels) != length(styles) || any(is.na(labels))) {
    rlang::abort("`labels` must be a character vector with one label per style.")
  }
  labels
}

.style_grid_render_style <- function(image, style, options, max_dimension) {
  if (identical(style, "original")) {
    return(.make_image_art(
      original = image,
      processed = image,
      plot = .magick_to_ggplot(image, background = "white"),
      image = image,
      type = "original",
      parameters = list(max_dimension = max_dimension),
      call = match.call()
    ))
  }

  args <- utils::modifyList(
    .style_grid_default_args(style, max_dimension),
    options[[style]] %||% list()
  )
  do.call(image_artify, c(list(image = image, type = style), args))
}

.style_grid_default_args <- function(style, max_dimension) {
  common <- list(max_dimension = max_dimension)
  switch(style,
    circle = c(list(n = 450, seed = 1), common),
    mosaic = c(list(tile_size = 12, background = "white"), common),
    outline = common,
    sketch = common,
    stipple = c(list(n = 1800, color = "original", point_size = c(0.25, 1.2), seed = 1), common),
    halftone = c(list(cell_size = 8, mode = "color", background = "white"), common),
    hex_mosaic = c(list(hex_size = 10, background = "white"), common),
    pop_art = c(list(panels = 4, seed = 1), common),
    duotone = c(list(shadow = "#243B53", highlight = "#FFB84D"), common),
    ascii = c(list(width = 64, output = "ggplot", color = TRUE, font_size = 2.6), common)
  )
}

.style_grid_tile <- function(
  art,
  label,
  tile_width,
  body_height,
  label_height,
  label_size,
  gap,
  background,
  label_color,
  dpi
) {
  image <- .style_grid_art_to_image(art, tile_width, body_height, background, dpi)
  if (label_height > 0) {
    label_image <- magick::image_blank(tile_width, label_height, color = background)
    label_image <- magick::image_annotate(
      label_image,
      text = label,
      size = label_size,
      color = label_color,
      gravity = "center"
    )
    image <- magick::image_append(c(image, label_image), stack = TRUE)
  }
  if (gap > 0) {
    image <- magick::image_border(image, color = background, geometry = sprintf("%sx%s", gap, gap))
  }
  image
}

.style_grid_art_to_image <- function(art, tile_width, body_height, background, dpi) {
  if (!is.null(art$image)) {
    image <- art$image
  } else {
    path <- tempfile(fileext = ".png")
    save_image_art(
      art,
      path,
      width = tile_width / dpi,
      height = body_height / dpi,
      dpi = dpi,
      overwrite = TRUE
    )
    image <- magick::image_read(path)
  }
  .style_grid_normalize_image(image, tile_width, body_height, background)
}

.style_grid_normalize_image <- function(image, width, height, background) {
  image <- magick::image_background(image, background, flatten = TRUE)
  image <- tryCatch(magick::image_trim(image), error = function(e) image)
  image <- magick::image_repage(image)
  image <- magick::image_resize(image, sprintf("%sx%s", width, height))
  magick::image_extent(
    image,
    sprintf("%sx%s", width, height),
    gravity = "center",
    color = background
  )
}

.style_grid_image <- function(tiles, ncol, background) {
  tile_info <- .image_info(tiles[[1]])
  blank <- magick::image_blank(tile_info$width, tile_info$height, color = background)
  rows <- split(seq_along(tiles), ceiling(seq_along(tiles) / ncol))
  row_images <- lapply(rows, function(index) {
    row_tiles <- tiles[index]
    if (length(row_tiles) < ncol) {
      row_tiles <- c(row_tiles, rep(list(blank), ncol - length(row_tiles)))
    }
    magick::image_append(do.call(c, row_tiles), stack = FALSE)
  })
  magick::image_append(do.call(c, row_images), stack = TRUE)
}
