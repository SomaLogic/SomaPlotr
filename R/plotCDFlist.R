#' Plot CDFs from Numeric List
#'
#' [plotCDFlist()] creates a series of cumulative distribution
#' function (CDF) plots from a numeric list object.
#'
#' @rdname plotCDF
#' @param .data A *named* list of numeric vectors (or a data.frame)
#'   containing the data to plot.
#' @param label Character. A label for the grouping
#'   variable, i.e. what the entries of the list represent.
#' @param ... Additional arguments passed to either
#'   [plotCDFbyGroup()] or [plotPDFbyGroup()], primarily one of:
#'   \itemize{
#'     \item `x.lab`
#'     \item `lty`
#'     \item `cols`
#'     \item `xlim`
#'     \item `fill`
#'     \item `ablines`
#'   }
#' @author Stu Field
#' @examples
#' # `plotCDFlist()`
#' x <- withr::with_seed(101,
#'   list(
#'     Group1 = rnorm(100),
#'     Group2 = rnorm(100, sd = 0.5),
#'     Group3 = rnorm(50, mean = 3, sd = 2)
#'   )
#' )
#' # warning: RFU values should all be positive!
#' plotCDFlist(x)
#' plotCDFlist(x, label = "SplitBy")
#' plotCDFlist(x, x.lab = "My x-axis", main = "Variable `x` CDF")
#' medians <- vapply(x, median, 0.0)
#' plotCDFlist(x, ablines = medians, main = "Variable `x` CDF")
#' @importFrom rlang sym !!
#' @importFrom tidyr gather
#' @export
plotCDFlist <- function(.data, label = "Group", main = "CDF by Group", ...) {

  if ( !inherits(.data, "list") ) {
    stop(
      "Please pass `.data` as a list of numeric vectors.",
      call. = FALSE
    )
  }

  label <- sym(label)
  data.frame(.data, check.names = FALSE) |>
    gather(key = !!label) |>
    plotCDFbyGroup(apt = "value", group.var = !!label, main = main, ...)
}
