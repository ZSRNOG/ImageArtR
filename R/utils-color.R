.as_hex <- function(red, green, blue, alpha = 255) {
  red <- pmin(255, pmax(0, round(red)))
  green <- pmin(255, pmax(0, round(green)))
  blue <- pmin(255, pmax(0, round(blue)))
  alpha <- pmin(255, pmax(0, round(alpha)))
  grDevices::rgb(red, green, blue, alpha = alpha, maxColorValue = 255)
}

.color_to_rgba <- function(color) {
  if (identical(tolower(color), "transparent")) {
    return(c(red = 0, green = 0, blue = 0, alpha = 0))
  }
  value <- grDevices::col2rgb(color, alpha = TRUE)[, 1]
  names(value) <- c("red", "green", "blue", "alpha")
  value
}

.rgb_to_hsv_table <- function(red, green, blue) {
  hsv <- grDevices::rgb2hsv(red, green, blue, maxColorValue = 255)
  tibble::tibble(
    hue = as.numeric(hsv["h", ]),
    saturation = as.numeric(hsv["s", ]),
    value = as.numeric(hsv["v", ])
  )
}

.rgb_to_hsl_table <- function(red, green, blue) {
  r <- red / 255
  g <- green / 255
  b <- blue / 255
  maxc <- pmax(r, g, b)
  minc <- pmin(r, g, b)
  delta <- maxc - minc
  lightness <- (maxc + minc) / 2

  saturation <- ifelse(
    delta == 0,
    0,
    delta / (1 - abs(2 * lightness - 1))
  )

  hue <- numeric(length(r))
  red_max <- maxc == r & delta != 0
  green_max <- maxc == g & delta != 0
  blue_max <- maxc == b & delta != 0
  hue[red_max] <- ((g[red_max] - b[red_max]) / delta[red_max]) %% 6
  hue[green_max] <- ((b[green_max] - r[green_max]) / delta[green_max]) + 2
  hue[blue_max] <- ((r[blue_max] - g[blue_max]) / delta[blue_max]) + 4
  hue <- hue / 6

  tibble::tibble(
    hue = hue,
    saturation = pmin(1, pmax(0, saturation)),
    value = lightness
  )
}

.nearest_palette_color <- function(colors, palette) {
  if (length(palette) == 0) {
    return(colors)
  }
  rgb_colors <- t(grDevices::col2rgb(colors))
  rgb_palette <- t(grDevices::col2rgb(palette))
  index <- vapply(seq_len(nrow(rgb_colors)), function(i) {
    diff <- sweep(rgb_palette, 2, rgb_colors[i, ], "-")
    which.min(rowSums(diff^2))
  }, integer(1))
  palette[index]
}

.interpolate_colors <- function(value, low, high, colorspace = c("RGB", "Lab")) {
  colorspace <- .arg_match(colorspace, c("RGB", "Lab"), "colorspace")
  value <- pmin(1, pmax(0, value))
  low_rgb <- grDevices::col2rgb(low) / 255
  high_rgb <- grDevices::col2rgb(high) / 255

  if (identical(colorspace, "Lab")) {
    endpoints <- grDevices::convertColor(
      rbind(as.numeric(low_rgb), as.numeric(high_rgb)),
      from = "sRGB",
      to = "Lab"
    )
    mixed <- outer(value, endpoints[2, ] - endpoints[1, ])
    mixed <- sweep(mixed, 2, endpoints[1, ], "+")
    rgb <- grDevices::convertColor(mixed, from = "Lab", to = "sRGB")
  } else {
    rgb <- matrix(c(
      low_rgb[1] + value * (high_rgb[1] - low_rgb[1]),
      low_rgb[2] + value * (high_rgb[2] - low_rgb[2]),
      low_rgb[3] + value * (high_rgb[3] - low_rgb[3])
    ), ncol = 3)
  }

  rgb <- matrix(pmin(1, pmax(0, as.vector(rgb))), ncol = 3)
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3])
}

.adjust_saturation <- function(red, green, blue, saturation = 1) {
  hsv <- grDevices::rgb2hsv(red, green, blue, maxColorValue = 255)
  hsv["s", ] <- pmin(1, pmax(0, hsv["s", ] * saturation))
  rgb <- grDevices::hsv(hsv["h", ], hsv["s", ], hsv["v", ])
  t(grDevices::col2rgb(rgb))
}
