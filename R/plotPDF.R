#' Plot PDF of Empirical Data
#'
#' [plotPDF()] creates a plot of the smoothed kernel density
#' estimate of the probability density function (PDF) for a numeric
#' vector of continuous data.
#'
#' @family cdf-pdf-plots
#' @inheritParams plotCDF
#' @param fill Logical. Should shaded colors beneath the curve be added?
#' @seealso [geom_density()]
#' @examples
#' # `plotPDF()`
#' x <- rnorm(100, mean = 5)
#' plotPDF(x)
#' plotPDF(x, col = "dodgerblue")
#' plotPDF(x, add.gauss = TRUE)
#' plotPDF(x, add.gauss = TRUE, fill = TRUE)
#'
#' @importFrom stats dnorm
#' @importFrom ggplot2 ggplot aes geom_density
#' @importFrom ggplot2 labs coord_cartesian geom_line stat_function
#' @export
plotPDF <- function(x, col = soma_colors$purple,
                    x.lab  = "values",
                    y.lab  = "Probability Density",
                    main   = "Kernel Density Estimate",
                    lty    = "solid",
                    fill   = FALSE,
                    add.gauss = FALSE) {

  p <- data.frame(x) |>
    ggplot(aes(x = x)) +
    geom_density(colour = col,
                 fill   = ifelse(fill, col, NA),
                 alpha  = ifelse(fill, 0.2, 1),
                 linewidth   = 0.75,
                 linetype = lty) +
    labs(title = main, y = y.lab, x = x.lab) +
    coord_cartesian() +
    theme_soma()

  if ( add.gauss ) {
    coeffs <- .fitGauss(x)
    pts    <- seq(min(x), max(x), length.out = 100)
    p <- p + geom_line(
      data = data.frame(x = pts,
                        y = stats::dnorm(pts,
                                         mean = coeffs$mu,
                                         sd = coeffs$sigma)),
      aes(x = x, y = y), col = "gray", linetype = "longdash")
  }

  p
}
