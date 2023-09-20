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
#'   mapply(mean = 3:5, n = c(10, 100, 1000), FUN = rnorm) |>
#'     setNames(paste0("Group", 1:3))
#' )
#' lengths(x)
#'
#' sapply(x, mean)
#'
#' # warning: RFU values should all be positive!
#' plotCDFlist(x)
#' plotCDFlist(x, label = "SplitBy")
#' plotCDFlist(x, x.lab = "My x-axis", main = "Variable `x` CDF")
#' medians <- vapply(x, median, 0.0)
#' plotCDFlist(x, ablines = medians, main = "Variable `x` CDF")
#' @importFrom rlang sym !!
#' @importFrom tidyr gather drop_na
#' @export
plotCDFlist <- function(.data, label = "Group", main = "CDF by Group", ...) {

  if ( !inherits(.data, "list") ) {
    stop(
      "Please pass `.data` as a list of numeric vectors.",
      call. = FALSE
    )
  }

  label <- sym(label)
  lapply(.data, "length<-", max(lengths(.data))) |>  # fix for jagged elements
    data.frame(check.names = FALSE) |>
    gather(key = !!label) |>
    drop_na(value) |>
    plotCDFbyGroup(apt = "value", group.var = !!label, main = main, ...)
}
