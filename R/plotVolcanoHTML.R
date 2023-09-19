#' Generate HTML-based Volcano Plot
#'
#' @inheritParams boxplotBeeswarm
#' @param data One of two object classes:
#' \describe{
#'   \item{`data.frame`:}{Containing log2-transformed fold-changes and
#'     corresponding p-values. In this case, an optional third column
#'     containing Target Names can be passed if specified in
#'     the `target.labels =` argument.}
#'   \item{`stat_table`:}{An object of class `c(stat_table, *_table)`.
#'    When this argument is provided, the `apt.data` argument _must_
#'    also be passed.}
#' }
#' @param FC An un-quoted variable name in `data` for the fold-changes.
#' @param p.vec An un-quoted variable name in `data` for the p-values.
#' @param cutoff Horizontal cutoff for coloring points. Defaults to Bonferroni
#'   corrected level of significance.
#' @param fc.cutoff Vertical cutoff for coloring coloring points
#'   (i.e. the fold-change). Defaults to doubling in fold-change.
#' @param ... Additional arguments passed to [plotly::plotly()].
#' @author Leigh Alexander, Stu Field
#' @seealso [plotly::plotly()]
#' @examples
#' # Dummy up a fake table with minimal variables
#' data <- SomaDataIO::example_data
#' apts <- SomaDataIO::getAnalytes(data)
#' df <- withr::with_seed(100,
#'    data.frame(
#'               AptName = sample(apts, 50),
#'               t.stat = runif(50, 10, 20),
#'               p.value = runif(50),
#'               signed.log2.fold.change = rnorm(50),
#'               Zscore = rnorm(50),
#'               # generate fake target names
#'               target = paste0("Target", "_", sample(LETTERS, 50, replace = TRUE),
#'                               sample(1:50, 50), "-",
#'                               sample(letters, 50, replace = TRUE))
#'              )
#' )
#'
#' # S3 `data.frame` method
#' # No Target labels -> `NA`
#' plotVolcanoHTML(df, cutoff = 0.1, fc.cutoff = 0.5)
#'
#' # Add Target labels -> `targets`
#' plotVolcanoHTML(df, cutoff = 0.1, fc.cutoff = 0.5, target.labels = target)
#'
#' # S3 `stat_table` method; calc.* family
#' # Dummy up a fake `stat_table` object
#' stbl <- list()
#' stbl$stat.table <- SomaDataIO::col2rn(df, "AptName")
#' stbl$test <- "Fake t-test"
#' stbl$data.dim <- dim(df)
#' stbl$y.response <- "LHS"
#' stbl$counts <- c(GroupA = 25, GroupB = 25)
#' stbl$log <- TRUE
#' stbl$data.frame <- "df"
#' class(stbl) <- c("stat_table", class(stbl))
#'
#' # Pick up apt.data
#' ad <- SomaDataIO::getAnalyteInfo(data)
#'
#' # Target names picked up from `apt.data`
#' plotVolcanoHTML(stbl, apt.data = ad, cutoff = 0.1, fc.cutoff = 0.5)
#'
#' # Change FC arg on fly -> `Zscore`
#' # Pass args via '...' -> `x.lab`
#' plotVolcanoHTML(stbl, apt.data = ad, cutoff = 0.1, fc.cutoff = 0.5,
#'                 FC = Zscore, x.lab = "Slope")
#' @importFrom stats setNames
#' @importFrom rlang enquo !!
#' @importFrom dplyr pull case_when ungroup select left_join
#' @export
plotVolcanoHTML <- function(data, FC, p.vec, cutoff, fc.cutoff, main, x.lab, ...) {
  UseMethod("plotVolcanoHTML")
}


#' S3 plotVolcanoHTML default method
#' @noRd
#' @export
plotVolcanoHTML.default <- function(data, ...) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(data)), call. = FALSE
  )
}


#' @describeIn plotVolcanoHTML
#' Plot method for objects of class `data.frame`.
#' @param target.labels Optional for the `data.frame` method. An un-quoted
#'   variable name in `data` for the Target Names.
#' @importFrom SomaDataIO rn2col col2rn
#' @export
plotVolcanoHTML.data.frame <- function(data, FC = signed.log2.fold.change, p.vec = p.value,
                                       cutoff = 0.05 / nrow(data),
                                       fc.cutoff = 1,
                                       main = NULL, x.lab = NULL,
                                       target.labels, ...) {

  if ( !"AptName" %in% names(data) ) {
    data <- rn2col(data, "AptName")
  }

  p_vec <- pull(data, !!enquo(p.vec))
  FC    <- pull(data, !!enquo(FC))

  if ( !missing(target.labels) ) {
    target.labels <- data |>
      pull(!!enquo(target.labels))
  } else {
    target.labels <- NA_character_
  }

  labels <- paste(
    "AptName:", data$AptName,
    "<br>TargetName:", target.labels,
    "<br>Fold Change:", format(2^FC, digits = 2),  # linear space
    "<br>p-value:", format(p_vec, digits = 2),
    "<br>"
  )

  if ( is.null(main) ) {
    main <- "Volcano Plot"
  }

  if ( is.null(x.lab) ) {
    x.lab <- "<i>log<sub>2</sub></i> Fold-Change"
  }

  p_vec <- -log10(p_vec)

  if ( all(FC >= 0) ) {
    warning(
      "It appears you are not passing log2-transformed ",
      "fold-change values. Please check.", call. = FALSE
    )
  }

  color.by <- case_when(
    (p_vec >= -log10(cutoff)) & (abs(FC) >= fc.cutoff) ~ "Significant & Fold Change",
    p_vec >= -log10(cutoff) ~ "Significant",
    abs(FC) >= fc.cutoff ~ "Fold Change",
    TRUE ~ "Non-Significant"
  )

  # matched to `plotVolcano` palette; sgf
  cols <- setNames(c(soma_colors$lightgrey,
                     soma_colors$purple,
                     soma_colors$lightgreen,
                     soma_colors$yellow),
                   c("Non-Significant",
                     "Significant",
                     "Fold Change",
                     "Significant & Fold Change"))

  plotly::plot_ly() |>
    plotly::add_trace(
      x = FC, y = p_vec,           # primary scatter plot of ps and fold changes
      color = color.by,
      colors = cols,
      mode = "markers", type = "scatter",
      marker = list(opacity = 0.7, size = 9),
      text = labels, ...) |>
    plotly::layout(
      title = main,
      xaxis = list(title = x.lab, showgrid = FALSE),
      yaxis = list(title = "-<i>log<sub>10</sub></i> p-value",
                   showgrid = FALSE),
      margin = list(b = 60, t = 60), ...)
}


#' @describeIn plotVolcanoHTML
#' Plot method for objects of class `stat_table`.
#' @param apt.data An `apt.data` object containing target information
#'   used to generate point labels in the final plot.
#'   This object can be obtained from a call to [getAnalyteInfo()]
#'   on the original data.
#' @importFrom SomaDataIO rn2col
#' @export
plotVolcanoHTML.stat_table <- function(data,
                                       FC = signed.log2.fold.change,
                                       p.vec = p.value,
                                       cutoff = 0.05 / nrow(data$stat.table),
                                       fc.cutoff = 1,
                                       main = NULL, x.lab = NULL,
                                       apt.data, ...) {

  calc_object <- data
  stat_table  <- rn2col(calc_object$stat.table, "AptName")

  levels    <- calc_object$counts
  ref_group <- names(levels)[1L]
  test      <- calc_object$test
  x_pred    <- if ( is.null(calc_object$response) ) {
    calc_object$x.predictor
  } else {
    calc_object$response
  }

  apt.data <- ungroup(apt.data) |>
    dplyr::select(AptName, TargetFullName)
  stat_table <- left_join(stat_table, apt.data, by = "AptName")
  p_vec <- pull(stat_table, !!enquo(p.vec))
  p_vec <- -log10(p_vec)
  FC    <- pull(stat_table, !!enquo(FC))
  target.labels <- stat_table$TargetFullName

  labels <- paste(
    "AptName:", stat_table$AptName,
    "<br>TargetName:", target.labels,
    "<br>Fold Change:", format(2^FC, digits = 2L),  # linear space
    "<br>p-value:", format(p_vec, digits = 2L),
    "<br>"
  )

  color.by <- case_when(
    (p_vec >= -log10(cutoff)) & (abs(FC) >= fc.cutoff) ~ "Significant & Fold Change",
    p_vec >= -log10(cutoff) ~ "Significant",
    abs(FC) >= fc.cutoff ~ "Fold Change",
    TRUE ~ "Non-Significant"
  )

  # matched to `plotVolcano` palette; sgf
  cols <- setNames(c(soma_colors$lightgrey,
                     soma_colors$purple,
                     soma_colors$lightgreen,
                     soma_colors$yellow),
                   c("Non-Significant",
                     "Significant",
                     "Fold Change",
                     "Significant & Fold Change"))

  if ( is.null(main) ) {
    if ( grepl("Student t-test|Wilcoxon|Logistic|Kolmog", test) ) {
      main <- sprintf(
        "%s (n = %s) vs. %s (n = %s)",
        names(levels)[1L], levels[[1L]], names(levels)[2L], levels[[2L]]
      )
    } else if ( grepl("Linear Regression|Spearman", test) ) {
      main <- sprintf("%s\n%s", test, x_pred)
    } else if ( grepl("Cox|AFT", test) ) {
      main <- sprintf(
        "Event: %s | Time: %s", calc_object$status, calc_object$time
      )
    }
  }

  if ( is.null(x.lab) ) {
    x.lab <- paste("<i>log</i><sub>2</sub> Fold Change<br>Reference Group:",
                   ref_group)
  } else {
    paste(x.lab, "<br>Reference Group:", ref_group)
  }

  if ( all(FC >= 0) ) {
    warning(
      "It appears you are not passing log2-transformed ",
      "fold-change values. Please check `data` argument.",
      call. = FALSE
    )
  }

  plotly::plot_ly() |>
    plotly::add_trace(
      x = FC, y = p_vec,
      color = color.by,
      colors = cols,
      mode = "markers", type = "scatter",
      marker = list(opacity = 0.7, size = 9),
      text = labels, ...) |>
    plotly::layout(
      title = main,
      xaxis = list(title = x.lab, showgrid = FALSE),
      yaxis = list(title = "-<i>log</i><sub>10</sub> p-value", showgrid = FALSE),
      margin = list(b = 60, t = 60), ...)
}
