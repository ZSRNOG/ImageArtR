.is_magick_image <- function(x) {
  inherits(x, "magick-image")
}

.is_cimg <- function(x) {
  inherits(x, "cimg") || isTRUE(imager::is.cimg(x))
}

.image_info <- function(image) {
  magick::image_info(image)[1, , drop = FALSE]
}

.limit_image_dimension <- function(image, max_dimension = 1600) {
  if (is.null(max_dimension)) {
    return(image)
  }
  .check_max_dimension(max_dimension)
  info <- .image_info(image)
  current_max <- max(info$width, info$height)
  if (current_max <= max_dimension) {
    return(image)
  }
  # 使用 ImageMagick 的 ">" 几何语法，只在图片过大时缩小。
  magick::image_resize(image, geometry = sprintf("%sx%s>", max_dimension, max_dimension))
}

.image_data_array <- function(image, channels = "rgba") {
  arr <- magick::image_data(image, channels = channels)
  storage.mode(arr) <- "integer"
  arr
}

.image_to_pixel_df <- function(image) {
  arr <- .image_data_array(image, "rgba")
  dims <- dim(arr)
  width <- dims[2]
  height <- dims[3]
  xy <- expand.grid(
    x = seq_len(width),
    y = seq_len(height),
    KEEP.OUT.ATTRS = FALSE
  )
  red <- as.vector(arr[1, , ])
  green <- as.vector(arr[2, , ])
  blue <- as.vector(arr[3, , ])
  alpha <- as.vector(arr[4, , ])
  tibble::tibble(
    x = xy$x,
    y = xy$y,
    red = red,
    green = green,
    blue = blue,
    alpha = alpha,
    hex = .as_hex(red, green, blue, alpha)
  )
}

.image_to_array <- function(image, channels = "rgba") {
  .image_data_array(image, channels = channels)
}

.image_to_pixels <- function(image) {
  .image_to_pixel_df(image)
}

.image_luminance <- function(image) {
  .grayscale_matrix(image)
}

.image_gradient <- function(image_or_luminance) {
  gray <- if (is.matrix(image_or_luminance)) {
    image_or_luminance
  } else {
    .image_luminance(image_or_luminance)
  }
  sobel <- .sobel_components(gray)
  .normalize01(sobel$magnitude)
}

.sample_pixel_color <- function(image, x, y) {
  arr <- .image_data_array(image, "rgba")
  width <- dim(arr)[2]
  height <- dim(arr)[3]
  x <- pmin(width, pmax(1, round(x)))
  y <- pmin(height, pmax(1, round(y)))
  red <- arr[cbind(rep(1L, length(x)), x, y)]
  green <- arr[cbind(rep(2L, length(x)), x, y)]
  blue <- arr[cbind(rep(3L, length(x)), x, y)]
  alpha <- arr[cbind(rep(4L, length(x)), x, y)]
  tibble::tibble(
    red = red,
    green = green,
    blue = blue,
    alpha = alpha,
    hex = .as_hex(red, green, blue, alpha)
  )
}

.sample_region_color <- function(arr, cx, cy, radius, method = c("mean", "median", "center")) {
  method <- .arg_match(method, c("mean", "median", "center"), "method")
  width <- dim(arr)[2]
  height <- dim(arr)[3]
  cx <- pmin(width, pmax(1, cx))
  cy <- pmin(height, pmax(1, cy))
  if (identical(method, "center") || radius <= 1) {
    ix <- pmin(width, pmax(1, round(cx)))
    iy <- pmin(height, pmax(1, round(cy)))
    return(.as_hex(arr[1, ix, iy], arr[2, ix, iy], arr[3, ix, iy], arr[4, ix, iy]))
  }

  x_min <- pmax(1, floor(cx - radius))
  x_max <- pmin(width, ceiling(cx + radius))
  y_min <- pmax(1, floor(cy - radius))
  y_max <- pmin(height, ceiling(cy + radius))
  grid <- tidyr::expand_grid(x = seq.int(x_min, x_max), y = seq.int(y_min, y_max))
  keep <- (grid$x - cx)^2 + (grid$y - cy)^2 <= radius^2
  grid <- grid[keep, , drop = FALSE]
  if (nrow(grid) == 0) {
    return(.sample_region_color(arr, cx, cy, radius = 1, method = "center"))
  }
  red <- arr[cbind(rep(1L, nrow(grid)), grid$x, grid$y)]
  green <- arr[cbind(rep(2L, nrow(grid)), grid$x, grid$y)]
  blue <- arr[cbind(rep(3L, nrow(grid)), grid$x, grid$y)]
  alpha <- arr[cbind(rep(4L, nrow(grid)), grid$x, grid$y)]
  fun <- if (identical(method, "mean")) mean else stats::median
  .as_hex(fun(red), fun(green), fun(blue), fun(alpha))
}

.regular_grid <- function(width, height, cell_size) {
  .check_positive_integer(cell_size, "cell_size")
  xs <- seq(1, width, by = cell_size)
  ys <- seq(1, height, by = cell_size)
  tidyr::expand_grid(x_min = xs, y_min = ys) |>
    dplyr::mutate(
      x_max = pmin(width, x_min + cell_size - 1),
      y_max = pmin(height, y_min + cell_size - 1),
      x = (x_min + x_max) / 2,
      y = (y_min + y_max) / 2,
      width = x_max - x_min + 1,
      height = y_max - y_min + 1
    )
}

.grid_cell_summary <- function(image, cell_size) {
  pixels <- .image_to_pixel_df(image)
  pixels$cell_x <- floor((pixels$x - 1) / cell_size)
  pixels$cell_y <- floor((pixels$y - 1) / cell_size)
  dplyr::summarise(
    dplyr::group_by(pixels, cell_x, cell_y),
    x_min = min(x),
    x_max = max(x),
    y_min = min(y),
    y_max = max(y),
    x = mean(x),
    y = mean(y),
    red = mean(red),
    green = mean(green),
    blue = mean(blue),
    alpha = mean(alpha),
    luminance = mean(0.299 * red + 0.587 * green + 0.114 * blue) / 255,
    .groups = "drop"
  )
}

.hex_grid <- function(width, height, hex_size) {
  .check_number(hex_size, "hex_size", min = 1)
  dx <- sqrt(3) * hex_size
  dy <- 1.5 * hex_size
  rows <- seq(0, ceiling(height / dy) + 1)
  purrr::map_dfr(rows, function(row) {
    y <- hex_size + row * dy
    offset <- if (row %% 2 == 0) 0 else dx / 2
    xs <- seq(hex_size + offset, width + dx, by = dx)
    tibble::tibble(x = xs, y = y, row = row)
  }) |>
    dplyr::filter(x >= -hex_size, x <= width + hex_size, y >= -hex_size, y <= height + hex_size) |>
    dplyr::mutate(id = dplyr::row_number(), radius = hex_size)
}

.hex_vertices <- function(hexes) {
  purrr::pmap_dfr(
    list(id = hexes$id, x = hexes$x, y = hexes$y, radius = hexes$radius, hex = hexes$hex),
    function(id, x, y, radius, hex) {
      theta <- seq(0, 2 * pi, length.out = 7)[-7] + pi / 6
      tibble::tibble(
        id = id,
        x = x + radius * cos(theta),
        y = y + radius * sin(theta),
        hex = hex
      )
    }
  )
}

.normalize_coordinates <- function(x, y, width, height) {
  tibble::tibble(
    x = pmin(width, pmax(1, x)),
    y = pmin(height, pmax(1, y))
  )
}

.quantize_colors <- function(hex, palette_size = NULL) {
  if (is.null(palette_size) || length(unique(hex)) <= palette_size) {
    return(hex)
  }
  rgb <- t(grDevices::col2rgb(hex))
  centers <- min(palette_size, nrow(unique(rgb)))
  if (centers <= 1) {
    center <- colMeans(rgb)
    return(rep(.as_hex(center[1], center[2], center[3]), length(hex)))
  }
  km <- .with_seed(1L, stats::kmeans(rgb, centers = centers))
  .as_hex(km$centers[km$cluster, 1], km$centers[km$cluster, 2], km$centers[km$cluster, 3])
}

.make_image_art <- function(original, processed, plot = NULL, image = NULL, type, parameters, call) {
  .new_image_art(
    original = original,
    processed = processed,
    plot = plot,
    image = image,
    type = type,
    parameters = parameters,
    call = call
  )
}

.clip01 <- function(x) {
  out <- pmin(1, pmax(0, as.vector(x)))
  if (is.matrix(x)) {
    return(matrix(out, nrow = nrow(x), ncol = ncol(x)))
  }
  out
}

.rgba_matrices_to_magick <- function(red, green, blue, alpha = NULL) {
  if (is.null(alpha)) {
    alpha <- matrix(255, nrow = nrow(red), ncol = ncol(red))
  }
  colors <- .as_hex(as.vector(red), as.vector(green), as.vector(blue), as.vector(alpha))
  raster <- matrix(colors, nrow = nrow(red), ncol = ncol(red))
  magick::image_read(grDevices::as.raster(raster))
}

.grayscale_matrix <- function(image) {
  arr <- .image_data_array(image, "rgba")
  red <- t(arr[1, , ]) / 255
  green <- t(arr[2, , ]) / 255
  blue <- t(arr[3, , ]) / 255
  0.299 * red + 0.587 * green + 0.114 * blue
}

.gray_to_magick <- function(mat, alpha = NULL) {
  value <- matrix(
    pmin(255, pmax(0, round(as.vector(mat) * 255))),
    nrow = nrow(mat),
    ncol = ncol(mat)
  )
  .rgba_matrices_to_magick(value, value, value, alpha = alpha)
}

.magick_to_ggplot <- function(image, background = "transparent") {
  info <- .image_info(image)
  raster <- grDevices::as.raster(image)
  fill <- if (identical(tolower(background), "transparent")) NA else background
  ggplot2::ggplot() +
    ggplot2::annotation_raster(
      raster,
      xmin = 0,
      xmax = info$width,
      ymin = 0,
      ymax = info$height
    ) +
    ggplot2::coord_fixed(
      xlim = c(0, info$width),
      ylim = c(0, info$height),
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = fill, color = NA),
      plot.background = ggplot2::element_rect(fill = fill, color = NA)
    )
}

.plot_background_theme <- function(background = "transparent") {
  fill <- if (identical(tolower(background), "transparent")) NA else background
  ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = fill, color = NA),
      plot.background = ggplot2::element_rect(fill = fill, color = NA),
      legend.position = "none"
    )
}

.shift_matrix <- function(mat, dy, dx, fill = 0) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  out <- matrix(fill, nrow = nr, ncol = nc)
  rows_from <- seq_len(nr)
  cols_from <- seq_len(nc)
  rows_to <- rows_from + dy
  cols_to <- cols_from + dx
  keep_rows <- rows_to >= 1 & rows_to <= nr
  keep_cols <- cols_to >= 1 & cols_to <= nc
  out[rows_to[keep_rows], cols_to[keep_cols]] <- mat[rows_from[keep_rows], cols_from[keep_cols]]
  out
}

.convolve2d <- function(mat, kernel) {
  kr <- nrow(kernel)
  kc <- ncol(kernel)
  if (kr %% 2 != 1 || kc %% 2 != 1) {
    rlang::abort("Convolution kernels must have odd dimensions.")
  }
  row_offsets <- seq_len(kr) - ceiling(kr / 2)
  col_offsets <- seq_len(kc) - ceiling(kc / 2)
  out <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  for (i in seq_along(row_offsets)) {
    for (j in seq_along(col_offsets)) {
      out <- out + kernel[i, j] * .shift_matrix(mat, row_offsets[i], col_offsets[j])
    }
  }
  out
}

.dilate_binary <- function(mat, radius = 1) {
  radius <- max(0, as.integer(radius))
  out <- mat > 0
  if (radius == 0) {
    return(out)
  }
  for (step in seq_len(radius)) {
    combined <- out
    for (dy in -1:1) {
      for (dx in -1:1) {
        combined <- combined | .shift_matrix(out, dy, dx, fill = FALSE)
      }
    }
    out <- combined
  }
  out
}

.erode_binary <- function(mat, radius = 1) {
  radius <- max(0, as.integer(radius))
  out <- mat > 0
  if (radius == 0) {
    return(out)
  }
  for (step in seq_len(radius)) {
    combined <- out
    for (dy in -1:1) {
      for (dx in -1:1) {
        combined <- combined & .shift_matrix(out, dy, dx, fill = FALSE)
      }
    }
    out <- combined
  }
  out
}

.binary_to_magick <- function(edges, background = "white", line_color = "black") {
  .check_color(background, "background")
  .check_color(line_color, "line_color")
  bg <- .color_to_rgba(background)
  fg <- .color_to_rgba(line_color)
  red <- matrix(bg["red"], nrow = nrow(edges), ncol = ncol(edges))
  green <- matrix(bg["green"], nrow = nrow(edges), ncol = ncol(edges))
  blue <- matrix(bg["blue"], nrow = nrow(edges), ncol = ncol(edges))
  alpha <- matrix(bg["alpha"], nrow = nrow(edges), ncol = ncol(edges))
  red[edges] <- fg["red"]
  green[edges] <- fg["green"]
  blue[edges] <- fg["blue"]
  alpha[edges] <- fg["alpha"]
  .rgba_matrices_to_magick(red, green, blue, alpha)
}
