\name{image_mosaic_art}
\alias{image_mosaic_art}
\title{Create Mosaic Art}
\usage{
image_mosaic_art(
  image,
  tile_size = 10,
  shape = c("square", "circle", "hexagon"),
  color_method = c("mean", "median", "dominant"),
  palette_size = NULL,
  border = FALSE,
  border_color = "white",
  border_width = 0.1,
  background = "transparent",
  max_dimension = 1000
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{tile_size}{Tile size in pixels.}
\item{shape}{Tile shape.}
\item{color_method}{Tile color summary method.}
\item{palette_size}{Optional number of colors used to compress the result.}
\item{border}{Draw tile borders?}
\item{border_color}{Border color.}
\item{border_width}{Border width.}
\item{background}{Plot background color.}
\item{max_dimension}{Maximum source image dimension before processing.}
}
\value{An \code{image_art} object.}
\description{
Divides an image into regular tiles and draws a square, circle, or hexagon
mosaic with \code{ggplot2}.
}
