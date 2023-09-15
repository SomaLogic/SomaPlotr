#' Plot Boxplots by Subarray (sample)
#'
#' Plots the distribution of of all analytes stratified
#' by subarray as a boxplot. In SomaScan (`soma_adat`) data format,
#' a subarray is typically a row or sample in the data.
#'
#' @family boxplots
#' @param .data A `soma_data` or data frame object created via a call to
#'   [read_adat()].
#' @param color.by A column name to color the subarrays by. Typically a meta
#'   data field in the adat such as `SlideId`.
#' @param labels Character. A column name of `adat` used to label each box.
#' @param do.log Logical. Should the data should be log10-transformed?
#' @param y.lim Numeric. Length 2. The upper- and lower-quantiles of the
#'   _total_ data used to determine the y-axis limits of the plot.
#'   If `NULL`, all points are shown.
#' @param apts An optional subset of analytes (as `AptNames`) to add on top
#'   of subarray boxplot.
#' @author Stu Field
#' @seealso [geom_boxplot()]
#' @examples
#' data <- SomaDataIO::example_data
#'
#' # Randomly select a small subset of samples
#' s_rn <- withr::with_seed(101, sample(rownames(data), 30L))
#' data <- data[s_rn, ]
#' boxplotSubarray(data)
#'
#' # Color by `SampleType` variable
#' boxplotSubarray(data, color.by = "SampleType")
#'
#' # Find the feature names of the corresponding hyb controls
#' hybs <- SomaDataIO::getAnalyteInfo(data) |>
#'   dplyr::filter(grepl("^Hybridization", Type)) |>
#'   dplyr::pull(AptName)
#'
#' # Pass hyb controls to `apts` arg; plots the points in boxes/whiskers
#' boxplotSubarray(data, color.by = "SampleType", apts = hybs)
#'
#' # Zoom to (20, 80) quantiles
#' boxplotSubarray(data, color.by = "SampleType", y.lim = c(0.2, 0.8))
#' @importFrom dplyr arrange select mutate row_number recode filter
#' @importFrom ggplot2 geom_boxplot scale_x_discrete labs coord_cartesian
#' @importFrom ggplot2 theme element_blank element_text aes geom_point
#' @importFrom rlang sym !! !!!
#' @importFrom stats quantile
#' @importFrom tidyr gather
#' @importFrom SomaDataIO getAnalytes
#' @export
boxplotSubarray <- function(.data, color.by = NULL, labels = "SampleId",
                            y.lim = NULL, do.log = TRUE, apts = NULL) {

  feats <- getAnalytes(.data)

  if ( length(labels) > 1L || !labels %in% names(.data) ) {
    stop(
      "The `labels =` argument should be a string ",
      "corresponding to the column name containing ",
      "the desired box labels.", call. = FALSE
    )
  }

  if ( "SampleType" %in% names(.data) ) {
    .data <- arrange(.data, SampleType)
  }

  ylab <- "RFU"

  if ( do.log ) {
    .data <- log10(.data)
    ylab <- "log10(RFU)"
  }

  if ( !is.null(y.lim) ) {
    y.lim <- data.matrix(.data[, feats, drop = FALSE]) |>
      as.numeric() |>
      quantile(probs = y.lim)
  }

  sym_label <- sym(labels)
  plot_data <- dplyr::select(.data, feats, !!sym_label) |>
    mutate(.id = row_number()) |>
    gather(key = "AptName", value = "RFU_values", -!!sym_label, -.id)

  if ( is.null(color.by) ) {
    plot_data$class <- ""
  } else {
    class_map <- setNames(.data[[color.by]], .data[[labels]])
    class_map <- class_map[unique(names(class_map))]
    plot_data <- mutate(plot_data, class = recode(!!sym_label, !!!class_map))
  }

  p <- plot_data |>
    ggplot(aes(x = as.character(.id), y = RFU_values, fill = class)) +
    geom_boxplot(notch = TRUE, alpha = 0.75, outlier.alpha = 0.2) +
    scale_fill_soma() +
    # ensures same order or samples
    scale_x_discrete(limits = factor(seq_len(nrow(.data))),
                     labels = .data[[labels]]) +
    labs(x = NULL, y = ylab) +
    coord_cartesian(ylim = y.lim) +
    theme(legend.title = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 1)) +
    NULL

  # call out specific aptamers
  if ( !is.null(apts) ) {
    p <- p + geom_point(data = dplyr::filter(plot_data, AptName %in% apts),
                        aes(x = .id, y = RFU_values, color = AptName), size = 2)
  }

  p
}
