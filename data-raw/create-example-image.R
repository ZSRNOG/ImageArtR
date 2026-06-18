# This script recreates inst/extdata/example.png for development use.
# It is intentionally not run during package checks.

width <- 240
height <- 180
path <- file.path("inst", "extdata", "example.png")

ellipse <- function(x, y, rx, ry, col, border = "#713042", lwd = 3, n = 120) {
  theta <- seq(0, 2 * pi, length.out = n)
  polygon(x + rx * cos(theta), y + ry * sin(theta), col = col, border = border, lwd = lwd)
}

arc <- function(x, y, rx, ry, start, end, col = "#713042", lwd = 3, n = 80) {
  theta <- seq(start, end, length.out = n)
  lines(x + rx * cos(theta), y + ry * sin(theta), col = col, lwd = lwd, lend = "round")
}

png_args <- list(filename = path, width = width, height = height, bg = "white")
if (capabilities("cairo")) {
  png_args$type <- "cairo"
}
do.call(png, png_args)
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, width), ylim = c(0, height), asp = 1)

rect(0, 0, width, height, col = "#BDEBFF", border = NA)
rect(0, 0, width, 42, col = "#9FDB7A", border = NA)
polygon(c(0, 55, 115, 172, width, width, 0), c(38, 52, 34, 50, 37, 0, 0),
  col = "#7CCB68", border = NA
)

ellipse(77, 138, 19, 27, col = "#F8A9BC", border = "#713042", lwd = 3)
ellipse(163, 138, 19, 27, col = "#F8A9BC", border = "#713042", lwd = 3)
ellipse(77, 136, 10, 17, col = "#FFD0DA", border = NA)
ellipse(163, 136, 10, 17, col = "#FFD0DA", border = NA)

ellipse(120, 111, 55, 45, col = "#F8A9BC", border = "#713042", lwd = 4)
ellipse(120, 74, 42, 34, col = "#3BA7C9", border = "#1E5B75", lwd = 4)
polygon(c(82, 158, 146, 94), c(82, 82, 39, 39), col = "#3BA7C9", border = "#1E5B75", lwd = 4)

lines(c(91, 62), c(78, 58), col = "#713042", lwd = 4, lend = "round")
lines(c(149, 178), c(78, 58), col = "#713042", lwd = 4, lend = "round")
ellipse(58, 56, 7, 6, col = "#F8A9BC", border = "#713042", lwd = 3)
ellipse(182, 56, 7, 6, col = "#F8A9BC", border = "#713042", lwd = 3)

lines(c(105, 96), c(42, 22), col = "#713042", lwd = 4, lend = "round")
lines(c(135, 144), c(42, 22), col = "#713042", lwd = 4, lend = "round")
ellipse(91, 20, 15, 6, col = "#2A5C7A", border = "#17364A", lwd = 3)
ellipse(149, 20, 15, 6, col = "#2A5C7A", border = "#17364A", lwd = 3)

ellipse(100, 119, 6, 8, col = "#1B1B1B", border = NA)
ellipse(140, 119, 6, 8, col = "#1B1B1B", border = NA)
ellipse(102, 122, 2, 2.5, col = "white", border = NA)
ellipse(142, 122, 2, 2.5, col = "white", border = NA)
ellipse(120, 101, 25, 16, col = "#FFD0DA", border = "#713042", lwd = 3)
ellipse(111, 101, 3.8, 4.8, col = "#713042", border = NA)
ellipse(129, 101, 3.8, 4.8, col = "#713042", border = NA)
ellipse(75, 103, 10, 8, col = "#F36C91", border = NA)
ellipse(165, 103, 10, 8, col = "#F36C91", border = NA)
arc(120, 91, 18, 10, pi + 0.2, 2 * pi - 0.2, col = "#713042", lwd = 3)

ellipse(92, 82, 4, 4, col = "#F8E46A", border = "#1E5B75", lwd = 2)
ellipse(148, 82, 4, 4, col = "#F8E46A", border = "#1E5B75", lwd = 2)
arc(195, 74, 13, 13, -0.15, 1.45 * pi, col = "#713042", lwd = 4)
ellipse(57, 145, 10, 10, col = "#FFD85A", border = NA)
dev.off()
