#' Plot CDFs of Continuous Variable by Grouping Variable
#'
#' [plotCDFbyGroup()] creates a plot with a series of cumulative distribution
#' function (CDF) plots stratified/split by a grouping
#' variable of (usually) meta data, e.g. `Response` grouping.
#'
#' @rdname plotCDF
#' @inheritParams boxplotBeeswarm
#' @param data A `soma_adat` or `data.frame` object containing RFU data.
#' @param apt Character. The name of a column in `data` to use in generating
#'   CDFs. Typically an aptamer measurement.
#' @param group.var An unquoted column name from `data` containing group labels.
#' @param xlim Numeric. Limits for the x-axis.
#'   See [coord_cartesian()].
#' @param lty Character. Passed to [geom_vline()]. See [ggtitle()].
#' @param ablines Numeric. A vector of x-axis position(s) for vertical
#'   lines to be added to the CDF or PDF.
#' @param include.counts Logical. Should class counts be added to the plot
#'   legend?
#' @author Stu Field
#' @examples
#' # `plotCDFbyGroup()`
#' data  <- SomaDataIO::example_data |> dplyr::filter(SampleType == "Sample")
#' anno  <- SomaDataIO::getTargetNames(SomaDataIO::getAnalyteInfo(data))
#' fsh   <- "seq.3032.11"
#' title <- anno[[fsh]]
#' plotCDFbyGroup(log10(data), apt = fsh, group.var = Sex, main = title)
#' lines <- split(log10(data[[fsh]]), data$Sex) |>
#'   vapply(median, double(1))
#' plotCDFbyGroup(log10(data), apt = fsh, group.var = Sex,
#'                ablines = lines, main = title)
#' plotCDFbyGroup(log10(data), apt = fsh, group.var = Sex,
#'                include.counts = TRUE, main = title)
#'
#' @importFrom rlang sym enquo quo_name !!
#' @importFrom dplyr pull select all_of
#' @importFrom ggplot2 ggplot aes ylab ggtitle stat_ecdf facet_wrap
#' @importFrom ggplot2 scale_color_manual scale_x_discrete scale_fill_manual
#' @export
plotCDFbyGroup <- function(data, apt, group.var, cols, xlim = NULL,
                           x.lab = bquote(italic(log)[10] ~ (RFU)),
                           main = apt, lty = "solid", ablines = NULL,
                           include.counts = FALSE) {

  if ( !is_chr(apt) ) {
    stop("`apt` must be a character string of length 1.")
  }

  group.var <- enquo(group.var)

  if ( !quo_name(group.var) %in% names(data) ) {
    stop(
      "The `group.var =` argument must be an unquoted name ",
      "contained in `data =` argument.", call. = FALSE
    )
  }

  tab <- table(data[[apt]])
  L   <- length(tab)

  if ( L < 5L ) {   # catch for non-continuous data
    stop(
      "Data appears non-continuous with (", value(L), ") categories ",
      "[", value(names(tab)), "]", call. = FALSE
    )
  }

  if ( !is.factor(pull(data, !!group.var)) ) {
    data[[quo_name(group.var)]] <- factor(data[[quo_name(group.var)]])
  }

  if ( any(data[[apt]] <= 0, na.rm = TRUE) ) {
    warning(
      "Non-positive values detected ...\n",
      "If RFU data, possibly multi-logging?",
      call. = FALSE
    )
  }

  p <- data |>
    dplyr::select(!!group.var, all_of(apt)) |>
    .refactorData() |>   # this is about ghost levels
    ggplot(aes(x = !!sym(apt), color = !!group.var)) +
    stat_ecdf(linewidth = 0.75, linetype = lty) +
    labs(x = x.lab, y = bquote(italic(P) ~ (X < x)), title = main) +
    coord_cartesian(xlim = xlim) +
    theme_soma() +
    NULL

  if ( missing(cols) ) {
    p <- p + scale_color_soma()
  } else {
    p <- p + scale_color_manual(values = unname(cols))
  }

  if ( !is.null(ablines) ) {
    p <- p + geom_vline(xintercept = ablines, linetype = "dashed")
  }

  if ( include.counts ) {
    counts <- pull(data, !!group.var) |> table(dnn = NULL)
    levs   <- sprintf("%s (n = %i)", names(counts), counts)
    p <- p + labs(subtitle = paste(levs, collapse = "\n"))
  }

  p
}
