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
#'   mapply(mean = 3:5, n = c(10, 100, 1000), FUN = rnorm) |>
#'     setNames(paste0("Group", 1:3))
#' )
#' lengths(x)
#'
#' sapply(x, mean)
#'
#' # warning: RFU values should all be positive!
#' plotPDFlist(x)
#' plotPDFlist(x, label = "SplitBy")
#' plotPDFlist(x, fill = TRUE)
#' plotPDFlist(x, x.lab = "My x-axis", main = "Variable `x` PDF")
#' medians <- vapply(x, median, 0.0)
#' plotPDFlist(x, ablines = medians, main = "Variable `x` PDF")
#' @importFrom rlang sym !!
#' @importFrom tidyr gather drop_na
#' @export
plotPDFlist <- function(.data, label = "Group", main = "PDF by Group", ...) {

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
    plotPDFbyGroup(apt = "value", group.var = !!label, main = main, ...)
}
