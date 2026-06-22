\name{image_style_grid}
\alias{image_style_grid}
\title{Create a Style Comparison Grid}
\usage{
image_style_grid(
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
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{styles}{Character vector of styles to render. Use \code{"original"} for
the source image, plus any style from \code{\link{list_art_styles}()}.}
\item{options}{Named list of per-style argument lists.}
\item{labels}{Optional labels shown under each tile.}
\item{ncol}{Number of columns. Defaults to a near-square layout.}
\item{tile_width}{Tile width in pixels before spacing.}
\item{tile_height}{Tile height in pixels before spacing.}
\item{label_height}{Label strip height in pixels.}
\item{label_size}{Label text size.}
\item{gap}{Space around each tile in pixels.}
\item{background}{Background color for the grid.}
\item{label_color}{Label text color.}
\item{max_dimension}{Maximum source image dimension used for preview renders.}
\item{dpi}{Plot rendering resolution used for ggplot-based styles.}
}
\value{An \code{image_art} object containing a \code{magick} image and a preview plot.}
\description{
Renders multiple ImageArtR styles for one input image and combines them into a
single labeled contact sheet. Per-style arguments can be supplied through the
\code{options} list.
}
\examples{
path <- system.file("extdata", "example.png", package = "ImageArtR")
grid <- image_style_grid(path, styles = c("original", "mosaic", "halftone"))
plot(grid)
}
