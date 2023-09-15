#' Plot a Double-Histogram
#'
#' Plots _two_ density histograms side-by-side.
#'
#' @inheritParams boxplotBeeswarm
#' @param data A 2-column `data.frame` of numeric
#'   data to be plotted as a paired density histogram.
#'   The column names of the `data.frame`
#'   are used as the group names for the plot.
#' @param label Character. A label for the grouping variable.
#' @param outline Logical. Black and white outlines of the histograms.
#' @param binwidth Numeric. Set the bin width for the histogram bars.
#'   See [geom_histogram()].
#' @author Stu Field
#' @examples
#' dat <- withr::with_seed(123, data.frame(a = rnorm(1000, 2, 0.3),
#'                                         b = rnorm(1000, 3, 0.3)))
#' plotDoubleHist(dat)
#' plotDoubleHist(dat, label = "Grouped By", main = "Two Distributions")
#' plotDoubleHist(dat, label = "Grouped By", main = "Black & White", outline = TRUE)
#' plotDoubleHist(dat, binwidth = 0.01)
#' plotDoubleHist(dat, binwidth = 0.01, outline = TRUE)
#' @importFrom ggplot2 geom_step geom_histogram scale_fill_manual
#' @importFrom ggplot2 theme theme_bw ggplot aes labs after_stat
#' @importFrom rlang sym !!
#' @importFrom tidyr gather
#' @export
plotDoubleHist <- function(data, cols, label = "Group",
                           x.lab = "value", binwidth = 0.05,
                           main = NULL, outline = FALSE) {

  stopifnot(ncol(data) == 2L)

  p <- data |>
    gather(key = !!label) |>
    ggplot(aes(x = value)) + {
      if ( outline ) {
        geom_step(aes(y = after_stat(density), linetype = !!sym(label)),
                  stat = "bin", binwidth = binwidth)
      } else {
        geom_histogram(aes(y = after_stat(density), fill = !!sym(label)),
                       binwidth = binwidth, alpha = 0.5,
                       position = "identity")
      }
    } +
    labs(title = main, x = x.lab) +
    theme_soma() +
    NULL

  if ( missing(cols) ) {
    p <- p + scale_fill_soma()
  } else {
    # ignored when outline = TRUE
    p <- p + scale_fill_manual(values = unname(cols))
  }

  p
}
