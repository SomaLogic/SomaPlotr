#' Plots an Empirical CDF
#'
#' [plotCDF()] creates a plot of the _empirical_ cumulative
#' distribution function for a numeric vector of continuous data. It is
#' similar to [ecdf()] with some visual modifications.
#'
#' @family cdf-pdf-plots
#' @inheritParams plotCDF
#' @inheritParams boxplotBeeswarm
#' @param x A numeric vector.
#' @param col Character. String for the color of the line.
#' @param lty Character. Passed to [geom_line()].
#' @param add.gauss Logical. Should a Gaussian fit of the
#'   data be plotted with the PDF?
#' @seealso [stat_ecdf()], [pnorm()]
#' @examples
#' # `plotCDF()`
#' x <- rnorm(100, mean = 5)
#' plotCDF(x)
#' plotCDF(x, col = "darkred")
#' plotCDF(x, col = "dodgerblue", add.gauss = TRUE)
#'
#' @importFrom ggplot2 ggplot element_rect element_text aes
#' @importFrom ggplot2 stat_ecdf labs coord_cartesian geom_line
#' @importFrom stats pnorm
#' @export
plotCDF <- function(x, col = soma_colors$purple,
                    x.lab  = "values",
                    y.lab  = bquote(~P(X < x)),
                    main   = "Empirical Cumulative Distribution Function",
                    lty    = "solid",
                    add.gauss = FALSE) {

  p <- data.frame(x) |>
    ggplot(aes(x = x)) +
    stat_ecdf(colour = col,
              linewidth = 0.75,
              linetype = lty) +
    labs(title = main, y = y.lab, x = x.lab) +
    coord_cartesian() +
    theme_soma()

  if ( add.gauss ) {
    coeffs <- .fitGauss(x)
    pts    <- seq(min(x), max(x), length.out = 50)
    p <- p + geom_line(
      data = data.frame(x = pts,
                        y = stats::pnorm(pts,
                                         mean = coeffs$mu,
                                         sd   = coeffs$sigma)),
      aes(x = x, y = y), col = "gray", linetype = "longdash")
  }

  p
}
