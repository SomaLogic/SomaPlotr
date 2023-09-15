#' Plot PDFs from Numeric List
#'
#' [plotPDFlist()] creates a series of plots of the smoothed kernel density
#' estimates of the probability density function (PDF) from a numeric list
#' object.
#'
#' @rdname plotPDF
#' @examples
#' # `plotPDFlist()`
#' x <- withr::with_seed(101,
#'   list(
#'     Group1 = rnorm(100),
#'     Group2 = rnorm(100, sd = 0.5),
#'     Group3 = rnorm(50, mean = 3, sd = 2)
#'   )
#' )
#' # warning: RFU values should all be positive!
#' plotPDFlist(x)
#' plotPDFlist(x, label = "SplitBy")
#' plotPDFlist(x, fill = TRUE)
#' plotPDFlist(x, x.lab = "My x-axis", main = "Variable `x` PDF")
#' medians <- vapply(x, median, 0.0)
#' plotPDFlist(x, ablines = medians, main = "Variable `x` PDF")
#' @importFrom rlang sym !!
#' @importFrom tidyr gather
#' @export
plotPDFlist <- function(.data, label = "Group", main = "PDF by Group", ...) {

  if ( !inherits(.data, "list") ) {
    stop(
      "Please pass `.data` as a list of numeric vectors.",
      call. = FALSE
    )
  }

  label <- sym(label)
  data.frame(.data, check.names = FALSE) |>
    gather(key = !!label) |>
    plotPDFbyGroup(apt = "value", group.var = !!label, main = main, ...)
}
