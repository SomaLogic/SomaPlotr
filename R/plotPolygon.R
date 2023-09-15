#' Plot a Polygon Shaded Area
#'
#' Plot a shaded area (interval) along a line via a polygon.
#' Typically for shading a confidence interval about a line.
#' This is just a convenient wrapper that makes more sense of the
#' [polygon()] function.
#'
#' @family base R
#' @param upper A list of length 2 containing the sequential `(x, y)` values
#'   for the _upper_ bound, where:
#'   \enumerate{
#'     \item entry 1 contains a vector of x-values,
#'     \item entry 2 contains a vector of y-values.
#'   }
#' @param lower A list of length 2 containing the sequential `(x, y)` values
#'   for the _lower_ bound, where:
#'   \enumerate{
#'     \item entry 1 contains a vector of x-values,
#'     \item entry 2 contains a vector of y-values.
#'   }
#' @param add Logical. Should the shaded area (polygon) be added
#'   to an existing plot?
#' @param col Color for the shaded area.
#' @param ... Additional arguments passed to the [plot()] function.
#'   Typically for arguments like: `main`, `xlab`, `ylim`, etc.
#' @return A polygon plot
#' @author Stu Field
#' @seealso [polygon()]
#' @examples
#' poly_data <- withr::with_seed(1, rnorm(1))
#' for ( i in 2:500 ) poly_data[i] <- poly_data[i - 1] + rnorm(1)
#' plotPolygon(list(1:length(poly_data), poly_data + 10),
#'            list(1:length(poly_data), poly_data - 10),
#'            xlab = "", ylab = "", main = "Polygon Wrapper")
#' addPolygon(list(1:length(poly_data), poly_data + 5),
#'            list(1:length(poly_data), poly_data - 5))
#' lines(poly_data, lwd = 2)
#'
#' # A full test wrapper (a simulation of genetic drift)
#' polywrap <- function(filename = NULL, seed = 1000) {
#'   figure(filename, width = 960, scale = 1.1)
#'   on.exit(close_figure(filename))
#'   poly_data <- withr::with_seed(seed, rnorm(1)) # initiate random
#'   for ( i in 2:500 ) {
#'     poly_data[i] <- poly_data[i - 1] + rnorm(1)
#'   }
#'   plotPolygon(list(1:length(poly_data), poly_data + 10),
#'               list(1:length(poly_data), poly_data - 10),
#'               xlab = "", ylab = "", main = "Polygon Wrapper")
#'   addPolygon(list(1:length(poly_data), poly_data + 5),
#'              list(1:length(poly_data), poly_data - 5),
#'              col = ggplot2::alpha("red", 0.5))
#'   lines(poly_data, lwd = 2)
#' }
#'
#' if ( interactive() ) {
#'   file <- tempfile("polygon-", fileext = ".png")
#'   polywrap(file)
#' }
#' @importFrom graphics plot
#' @export
plotPolygon <- function(upper, lower, add = FALSE,
                        col = ggplot2::alpha("blue", 0.5), ...) {

  stopifnot(
    "`upper` must be a `list` of length 2." = is.list(upper) & length(upper) == 2L,
    "`lower` must be a `list` of length 2." = is.list(lower) & length(lower) == 2L
  )

  x1 <- upper[[1L]]   # line1 x-values
  y1 <- upper[[2L]]   # line1 y-values
  x2 <- lower[[1L]]   # line2 x-values
  y2 <- lower[[2L]]   # line2 y-values

  if ( !add ) {
    graphics::plot(x1, y1, type = "n",
                   ylim = c(min(y1, y2), max(y1, y2)),
                   xlim = c(min(x1, x2), max(x1, x2)), ...)
  }
  graphics::polygon(c(x1, rev(x2)), c(y1, rev(y2)), border = NA, col = col)
}

#' @describeIn plotPolygon
#' A convenient wrapper for _adding_ a polygon to an existing plot
#' which simply hard codes the `add = TRUE` argument and passes additional
#' arguments to [plotPolygon()].
#' @export
addPolygon <- function(upper, lower, col = ggplot2::alpha("blue", 0.5), ...) {
  plotPolygon(upper = upper, lower = lower, col = col, add = TRUE, ...)
}
