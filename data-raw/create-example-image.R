# This script recreates inst/extdata/example.png for development use.
# It is intentionally not run during package checks.

width <- 160
height <- 110
path <- file.path("inst", "extdata", "example.png")

img <- grDevices::as.raster(matrix("#FFFFFF", nrow = height, ncol = width))
for (y in seq_len(height)) {
  for (x in seq_len(width)) {
    red <- round(40 + 160 * (x - 1) / max(1, width - 1))
    green <- round(70 + 120 * (y - 1) / max(1, height - 1))
    blue <- round(210 - 90 * (x - 1) / max(1, width - 1))
    img[y, x] <- grDevices::rgb(red, green, blue, maxColorValue = 255)
  }
}

png(path, width = width, height = height)
par(mar = c(0, 0, 0, 0))
plot.new()
rasterImage(img, 0, 0, 1, 1)
symbols(0.30, 0.64, circles = 0.18, inches = FALSE, add = TRUE, bg = "#FFB446D2", fg = NA)
rect(0.54, 0.30, 0.83, 0.78, col = "#1E78B4D2", border = NA)
polygon(c(0.69, 0.91, 0.96), c(0.24, 0.73, 0.16), col = "#E63C5AD7", border = NA)
t <- seq(0, 1, length.out = 40)
bx <- (1 - t)^3 * 0.05 + 3 * (1 - t)^2 * t * 0.31 + 3 * (1 - t) * t^2 * 0.59 + t^3 * 0.95
by <- (1 - t)^3 * 0.16 + 3 * (1 - t)^2 * t * 0.45 + 3 * (1 - t) * t^2 * -0.02 + t^3 * 0.29
lines(bx, by, lwd = 3)
dev.off()
