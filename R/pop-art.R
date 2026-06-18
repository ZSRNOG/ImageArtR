.pop_art_palettes <- list(
  warhol = c("#F72585", "#7209B7", "#3A0CA3", "#4CC9F0"),
  citrus = c("#FFBE0B", "#FB5607", "#FF006E", "#8338EC"),
  comic = c("#FFE45E", "#FF6392", "#00B4D8", "#2EC4B6"),
  neon = c("#00F5D4", "#F15BB5", "#FEE440", "#9B5DE5"),
  poster = c("#F94144", "#F3722C", "#F9C74F", "#277DA1"),
  candy = c("#F8BBD0", "#B8F2E6", "#AED9E0", "#FFA69E")
)

#' List built-in pop art palettes
#'
#' @return A tibble with palette names and color vectors.
#' @export
#'
#' @examples
#' list_pop_art_palettes()
list_pop_art_palettes <- function() {
  tibble::tibble(
    name = names(.pop_art_palettes),
    colors = unname(.pop_art_palettes)
  )
}

#' Create pop art panels from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param panels Number of panels. Supported values are 1, 2, and 4.
#' @param layout Optional `c(rows, cols)` layout.
#' @param palette Optional custom palette vector or list of palette vectors.
#' @param palette_method Palette source: preset, random, or image.
#' @param levels Number of brightness levels.
#' @param outline Add edge outlines?
#' @param outline_color Outline color.
#' @param outline_width Outline width in pixels.
#' @param contrast Contrast multiplier applied before posterization.
#' @param seed Optional random seed. The previous user seed is restored.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_pop_art(path, panels = 4, seed = 123)
#' plot(result)
image_pop_art <- function(
  image,
  panels = 4,
  layout = NULL,
  palette = NULL,
  palette_method = c("preset", "random", "image"),
  levels = 4,
  outline = TRUE,
  outline_color = "black",
  outline_width = 1,
  contrast = 1.5,
  seed = NULL,
  max_dimension = 1000
) {
  call <- match.call()
  palette_method <- .arg_match(palette_method, c("preset", "random", "image"), "palette_method")
  if (!panels %in% c(1, 2, 4)) {
    rlang::abort("`panels` must be one of 1, 2, or 4.")
  }
  .check_number(levels, "levels", min = 2, integer = TRUE)
  .check_bool(outline, "outline")
  .check_color(outline_color, "outline_color")
  .check_number(outline_width, "outline_width", min = 0)
  .check_number(contrast, "contrast", min = 0.1, max = 10)
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  gray <- .clip01((.image_luminance(working) - 0.5) * contrast + 0.5)
  edges <- if (outline) {
    .adjust_edge_width(.edge_binary_from_matrix(gray, method = "sobel"), outline_width, simplify = 0)
  } else {
    matrix(FALSE, nrow = nrow(gray), ncol = ncol(gray))
  }
  panel_palettes <- .with_seed(seed, {
    .pop_panel_palettes(working, panels, levels, palette, palette_method)
  })

  panel_images <- purrr::map(panel_palettes, function(pal) {
    .pop_panel_image(gray, edges, pal, outline_color)
  })
  layout <- .pop_layout(panels, layout)
  rows <- purrr::map(seq_len(layout[1]), function(row) {
    start <- (row - 1) * layout[2] + 1
    magick::image_append(do.call(c, panel_images[start:(start + layout[2] - 1)]), stack = FALSE)
  })
  combined <- magick::image_append(do.call(c, rows), stack = TRUE)

  .make_image_art(
    original = original,
    processed = list(palettes = panel_palettes, layout = layout),
    plot = .magick_to_ggplot(combined),
    image = combined,
    type = "pop_art",
    parameters = list(
      panels = panels,
      layout = layout,
      palette_method = palette_method,
      levels = levels,
      outline = outline,
      outline_color = outline_color,
      outline_width = outline_width,
      contrast = contrast,
      seed = seed,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.pop_layout <- function(panels, layout) {
  if (is.null(layout)) {
    return(switch(as.character(panels),
      `1` = c(1L, 1L),
      `2` = c(1L, 2L),
      `4` = c(2L, 2L)
    ))
  }
  if (!is.numeric(layout) || length(layout) != 2 || any(is.na(layout)) || prod(layout) != panels) {
    rlang::abort("`layout` must be NULL or numeric c(rows, cols) with product equal to `panels`.")
  }
  as.integer(layout)
}

.pop_panel_palettes <- function(image, panels, levels, palette, palette_method) {
  if (!is.null(palette)) {
    palettes <- if (is.list(palette)) palette else list(palette)
    palettes <- rep(palettes, length.out = panels)
    return(lapply(palettes, .expand_palette, n = levels))
  }
  if (identical(palette_method, "image")) {
    image_pal <- tryCatch(
      extract_image_palette(image, n = max(levels, panels * 2))$hex,
      error = function(e) {
        extract_image_palette(
          image,
          n = max(levels, panels * 2),
          remove_transparent = FALSE
        )$hex
      }
    )
    return(lapply(seq_len(panels), function(i) .expand_palette(.rotate_vector(image_pal, i - 1), levels)))
  }
  if (identical(palette_method, "random")) {
    names <- sample(names(.pop_art_palettes), panels, replace = panels > length(.pop_art_palettes))
  } else {
    names <- rep(names(.pop_art_palettes), length.out = panels)
  }
  lapply(.pop_art_palettes[names], .expand_palette, n = levels)
}

.rotate_vector <- function(x, n) {
  if (length(x) == 0 || n == 0) {
    return(x)
  }
  n <- n %% length(x)
  c(utils::tail(x, n), utils::head(x, length(x) - n))
}

.expand_palette <- function(palette, n) {
  palette <- stats::na.omit(palette)
  if (length(palette) < 2) {
    rlang::abort("A pop art palette must contain at least two colors.")
  }
  grDevices::colorRampPalette(palette)(n)
}

.pop_panel_image <- function(gray, edges, palette, outline_color) {
  bins <- pmin(length(palette), pmax(1, ceiling(gray * length(palette))))
  colors <- palette[bins]
  rgb <- grDevices::col2rgb(as.vector(colors), alpha = TRUE)
  red <- matrix(rgb[1, ], nrow = nrow(gray), ncol = ncol(gray))
  green <- matrix(rgb[2, ], nrow = nrow(gray), ncol = ncol(gray))
  blue <- matrix(rgb[3, ], nrow = nrow(gray), ncol = ncol(gray))
  alpha <- matrix(255, nrow = nrow(gray), ncol = ncol(gray))
  if (any(edges)) {
    outline <- .color_to_rgba(outline_color)
    red[edges] <- outline["red"]
    green[edges] <- outline["green"]
    blue[edges] <- outline["blue"]
    alpha[edges] <- outline["alpha"]
  }
  .rgba_matrices_to_magick(red, green, blue, alpha)
}
