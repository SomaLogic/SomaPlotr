#' Calculate Missingness of Clinical Meta Data
#'
#' Calculate the "missingness" (`NAs`) of clinical meta data of an ADAT.
#' For the S3 plotting method, see [plot.Map()].
#'
#' @family Calc Map
#' @param data A data frame (or `soma_adat`) of _only_ the meta data columns
#'   (i.e. no analytes).
#' @param include.pattern Character (optional). A regular expression string
#'   used in a [grep()] call to include matching column names.
#'   Defaults to _include_ all column names in the meta data (".").
#' @param exclude.pattern Character (optional). A regular expression string
#'   used in a [grep()] call to _exclude_ matching column names.
#' @return A list of class `c("missingness_map", "Map")` containing:
#'   \item{matrix}{A boolean matrix of `TRUE/FALSE` whether each sample is in
#'     missingness according the the stated criteria.}
#'   \item{names}{A character vector containing the names of the meta data
#'     columns.}
#'   \item{rows.by.freq}{A logical indicating if the samples are ordered
#'     by missingness frequency. Currently always FALSE.}
#'   \item{legend.sub}{A character string containing the plot legend subtitle.}
#'   \item{title}{A character string containing the plot title.}
#'   \item{x.lab}{A character string containing the plot x-axis label.}
#' @author Stu Field
#' @examples
#' sample.adat <- SomaDataIO::example_data
#' meta <- sample.adat[, SomaDataIO::getMeta(sample.adat)]
#'
#' # random assign NAs
#' cols <- rep(1:ncol(meta), each = 3)
#' rows <- as.integer(replicate(ncol(meta), sample(1:nrow(meta), 3)))
#' meta[cbind(rows, cols)] <- NA
#'
#' mm <- calcMissingnessMap(meta)
#' class(mm)
#'
#' # S3 print method
#' mm
#' @importFrom SomaDataIO is.apt
#' @export
calcMissingnessMap <- function(data, include.pattern = ".", exclude.pattern = NULL) {

  if ( any(is.apt(names(data))) ) {
    warning(
      "Non-meta data fields detected in data frame ... may ",
      "cause x-axis issues during plotting", call. = FALSE
    )
  }

  good_names <- grep(include.pattern, names(data), value = TRUE)

  if ( !is.null(exclude.pattern) ) {
    good_names <- grep(exclude.pattern, good_names, value = TRUE, invert = TRUE)
  }

  ret            <- list(matrix = is.na(data[, good_names]))
  ret$names      <- good_names
  ret$rows.by.freq <- FALSE
  ret$class.tab  <- NA
  ret$legend.sub <- "MetaData"
  ret$title      <- "Meta Data Missingness Map"
  ret$x.lab      <- ""
  class(ret)     <- c("missingness_map", "Map", class(ret))
  invisible(ret)
}


#' @describeIn calcMissingnessMap
#' There is a S3 print method for `"missingness_map"`.
#' @param x An object of class `"missingness_map"`.
#' @param ... Arguments for S3 print methods.
#' @export
print.missingness_map <- function(x, ...) {
  writeLines(
    cli_rule("SomaLogic Missingness Map", line = "double", line_col = "magenta")
  )
  key <- c(
    "Missingness Map dim",
    "Title",
    "Class Table",
    "Legend Sub-title") |> .pad(25)
  value <- c(
    value(paste(dim(x$matrix), collapse = " x ")),
    value(x$title),
    x$class.tab,
    value(x$legend.sub)
  )
  writeLines(paste(" ", key, value))
  writeLines(cli_rule(line = "double", line_col = "green"))
  invisible(x)
}

#' S3 plot methods for class missingness_map
#' @noRd
#' @export
plot.missingness_map <- function(x, ...) {
  NextMethod("plot", type = "missingness")
}
