#' Plot PDFs of Continuous Variable by a Grouping Variable
#'
#' [plotPDFbyGroup()] creates a plot with a series of smoothed
#' density estimates of the probability density function (PDF)
#' stratified/split by a grouping variable of (usually) meta data,
#' e.g. `Response` grouping.
#'
#' @rdname plotPDF
#' @inheritParams plotCDFbyGroup
#' @examples
#' # `plotPDFbyGroup()`
#' data  <- SomaDataIO::example_data |> dplyr::filter(SampleType == "Sample")
#' anno  <- SomaDataIO::getTargetNames(SomaDataIO::getAnalyteInfo(data))
#' fsh   <- "seq.3032.11"
#' title <- anno[[fsh]]
#' plotPDFbyGroup(log10(data), apt = fsh, group.var = Sex, main = title)
#' plotPDFbyGroup(log10(data), apt = fsh, group.var = Sex,
#'                fill = TRUE, main = title)
#' lines <- split(log10(data[[fsh]]), data$Sex) |>
#'   vapply(median, double(1))
#' plotPDFbyGroup(log10(data), apt = fsh, group.var = Sex,
#'                fill = TRUE, ablines = lines, main = title)
#'
#' @importFrom rlang enquo quo_name sym !!
#' @importFrom dplyr pull select
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_x_discrete
#' @importFrom ggplot2 ylab facet_wrap ggtitle geom_line
#' @export
plotPDFbyGroup <- function(data, apt, group.var, cols, xlim = NULL,
                           x.lab = bquote(italic(log)[10] ~ (RFU)),
                           main = apt, lty = "solid", fill = FALSE,
                           ablines = NULL, include.counts = FALSE) {

  group.var <- enquo(group.var)

  if ( !quo_name(group.var) %in% names(data) ) {
    stop(
      "The `group.var =` argument must be an unquoted name ",
      "contained in the `data =` argument.", call. = FALSE
    )
  }

  tab <- table(data[[apt]])
  L   <- length(tab)

  if ( L < 5L ) {  # catch for non-continuous data
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

  if ( fill ) {
    group_var <- group.var
  } else {
    group_var <- NULL
  }

  p <- data |>
    dplyr::select(!!group.var, apt) |>
    .refactorData() |>     # this is about ghost levels
    ggplot(aes(x = !!sym(apt),
               color = !!group.var)) +
    geom_density(aes(fill  = !!group_var),
                 alpha = ifelse(fill, 0.2, 1),
                 linewidth = ifelse(fill, 0.1, 0.75),
                 linetype = lty) +
    labs(y = "Probability Density", title = main, x = x.lab) +
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
