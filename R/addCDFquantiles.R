#' Add Quantile Lines to CDF Plot
#'
#' Add quantile/percentile lines to an existing `ggplot` cumulative
#' distribution function, typically one created using [plotCDF()].
#'
#' @family Adding Graphics
#' @param x Numeric. A vector of data corresponding to the
#'   data used to create the CDF.
#' @param p Numeric. Value(s) between 0-1 for the quantiles. Can be
#'   `length > 1` if more than one quantile is desired.
#' @param col Color of the lines/quantiles to be added;
#'   _must_ be the same length as `p`.
#' @param lty Character. The line type for the added quantile lines.
#'   Can be a single value if the same line type is desired for all quantiles.
#'   Can be a vector (the same length as `p`) if different line types are
#'   desired for each quantile.
#' @author Stu Field, Amanda Hiser
#' @seealso [quantile()], [plotCDF()], [annotate()]
#' @examples
#' rdat <- withr::with_seed(101, rnorm(100))
#' plt <- plotCDF(rdat, main = "Random Gaussian", x.lab = "Data")
#' plt + addCDFquantiles(rdat, p = 0.9, col = soma_colors2$blue)
#'
#' # Multiple quantiles can be added in one call if a vector is
#' # provided for both 'p' and 'col' (and optionally 'lty')
#' plt +
#'   addCDFquantiles(rdat,
#'                   p   = c(0.9, 0.5),
#'                   col = c(soma_colors2$blue, soma_colors2$teal))
#'
#' # The same effect can be achieved by adding each quantile individually
#' plt + addCDFquantiles(rdat, p = 0.9, col = soma_colors2$blue) +
#'   addCDFquantiles(rdat, p = 0.5, col = soma_colors2$teal) +
#'   addCDFquantiles(rdat, p = 0.25, col = soma_colors2$pink)
#' @importFrom stats quantile
#' @importFrom ggplot2 annotate
#' @export
addCDFquantiles <- function(x, p = 0.5, col, lty = "dashed") {

  stopifnot(
    "The value(s) provided for p must be between 0 and 1." = p < 1.0,
    "The length of col must be the same length as p."      = length(p) == length(col),
    "The length of lty cannot be longer than p."           = length(lty) <= length(p)
  )

  q <- quantile(x, p = p)
  l <- length(p)

  # If only 1 line type is specified but length p > 1,
  # the lty vector length must match that of p
  lty <- if ( length(lty) == 1L && l > 1L ) {
    rep_len(lty, l * 2)
  } else {
    c(rbind(lty, rep_len(lty, l)))
  }

  annotate("segment",
    # Coordinates to place the quantiles must be generated
    # for each element of p (when length p > 1)
    x        = as.numeric(rbind(min(x), rep_len(q, l))),
    xend     = as.numeric(rbind(q, rep_len(q, l))),
    y        = as.numeric(rbind(p, rep_len(p, l))),
    yend     = as.numeric(rbind(p, rep_len(0, l))),
    linetype = lty,
    colour   = rep(col, each = 2)
  )
}
