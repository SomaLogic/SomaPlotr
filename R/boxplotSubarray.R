#' Plot Boxplots by Subarray (Sample)
#'
#' Plots the distribution of all analytes, stratified by subarray, as a
#' boxplot. These plots are intended to be used as a quality control
#' visualization tool for SomaScan assay runs. In SomaScan (`soma_adat`) data
#' format, the term "subarray" is analogous to sample, and typically indicates
#' a row in the data.
#'
#' @family boxplots
#' @param .data A `soma_data` or data frame object, created from a SomaScan
#'   ADAT file, via a call to [read_adat()]. This object must contain the
#'   following columns: `PlateId`, `SampleId`, `SampleType`, `SampleMatrix`,
#'   `Barcode2d`, `SlideId`, `Subarray`, and `HybControlNormScale`.
#' @param color.by Character. A column name to color the subarrays (samples)
#'   by. This is typically a sample processing or clinical data field in the
#'   ADAT such as `SlideId`.
#' @param labels Character. The column name of `.data` used to label each box.
#' @param do.log Logical. Should the data be log10-transformed?
#' @param y.lim Numeric. Length 2. The upper- and lower-quantiles of the
#'   _total_ data used to determine the y-axis limits of the plot.
#'   If `NULL`, all points are shown.
#' @param apts Optional. A subset of analytes (as `AptNames`) to add as points
#'   on top of the subarray boxplot.
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
#'
#' # Group by an additional variable
#' boxplotSubarray(data, color.by = "SampleType") +
#'   ggplot2::facet_wrap(~PlateId)
#' @importFrom dplyr all_of arrange select mutate row_number
#' @importFrom dplyr recode filter
#' @importFrom ggplot2 geom_boxplot scale_x_discrete labs coord_cartesian
#' @importFrom ggplot2 theme element_blank element_text aes geom_point
#' @importFrom rlang sym !! !!!
#' @importFrom stats quantile
#' @importFrom tidyr pivot_longer
#' @importFrom SomaDataIO getAnalytes
#' @export
boxplotSubarray <- function(.data, color.by = NULL, labels = "SampleId",
                            y.lim = NULL, do.log = TRUE, apts = NULL) {

  reqd_cols <- c(known_chr, known_dbl)
  feats <- getAnalytes(.data)

  if ( !all(reqd_cols %in% names(.data)) ) {
    msng <- setdiff(reqd_cols, names(.data))
    stop(
      "The object provided to the `.data =` argument is missing ",
      "the following required feature columns: ",
      paste(msng, collapse = ", "), call. = FALSE
    )
  }

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
  plot_data <- dplyr::select(.data, all_of(feats), !!sym_label,
                             all_of(c(reqd_cols))) |>
    mutate(.id = row_number()) |>
    pivot_longer(names_to = "AptName", values_to = "RFU_values",
                 values_transform = list(AptName = as.character,
                                         RFU_values = as.numeric),
                 -c(!!sym_label, .id, reqd_cols))

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
    theme_soma() +
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
