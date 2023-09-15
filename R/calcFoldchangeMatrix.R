#' Calculate Fold-Change Matrix
#'
#' Calculate the column-wise fold-change matrix of an ADAT.
#' The clustering method "OLO" (Optimal-leaf-ordering)
#' produces an optimal leaf ordering with respect to the
#' minimizing the sum of the distances along the (Hamiltonian)
#' path connecting the leaves in the given order. The time
#' complexity of the algorithm is O(n^3). Note that
#' non-finite distance values are not allowed.
#'
#' For the S3 plotting method, see [plot.Map()].
#'
#' @family Calc Map
#' @param data A `soma_adat` object containing RFU feature data.
#' @param anno_tbl An annotations table produced via [getAnalyteInfo()].
#'   Used to calculate analyte dilutions for the matrix column ordering.
#'   If `NULL`, a table is generated internally from `data` (if possible), and
#'   the analytes are plotted in dilution order.
#' @param apt.order Character. How should the columns (features) be ordered. By
#'   dilution mix, clustered, or as is in the `data` object?
#' @param sample.order Either a character string indicating the column name
#'   with entries to be used to order the data frame rows, or a numeric vector
#'   representing the order of the data frame rows. The
#'   default (`NULL`) leaves the row ordering as it is in `data`.
#' @param max Max value for the range to evaluate. Defaults to 3 which is
#'   typically good for most RFU data.
#' @param threshold A threshold for the fold-change values. Values less than
#'   the threshold are set to zero. Threshold must be less than `"max"`.
#' @return A list of class `c("foldchange_matrix", "Map")` containing:
#'   \item{matrix}{A matrix containing the protein fold-change values of each
#'     sample.}
#'   \item{x.lab}{A character string containing the plot x-axis label.}
#'   \item{title}{A character string containing the plot title.}
#'   \item{sample.order}{A numeric vector representing the order of the data
#'     frame rows.}
#'   \item{legend.sub}{A character string containing the plot legend subtitle.}
#' @author Stu Field
#' @examples
#' sample.adat <- SomaDataIO::example_data
#' fc <- calcFoldchangeMatrix(sample.adat)
#' class(fc)
#'
#' # S3 print method
#' fc
#'
#' # Sample Order
#' # specified by user
#' fc <- calcFoldchangeMatrix(sample.adat, sample.order = 192:1)  # by row indices
#' fc$sample.order
#'
#' # specified by field
#' fc <- calcFoldchangeMatrix(sample.adat, sample.order = "Sex")
#' fc$sample.order
#'
#' # specified by 2 fields in ADAT
#' fc <- calcFoldchangeMatrix(sample.adat,
#'                            sample.order = c("Sex", "TimePoint"))
#' fc$sample.order
#' @importFrom stats dist
#' @importFrom SomaDataIO getAnalyteInfo getAnalytes
#' @export
calcFoldchangeMatrix <- function(data, anno_tbl = NULL,
                                 apt.order = c(NA, "dilution", "cluster"),
                                 sample.order = NULL, max = 3,
                                 threshold = NULL) {

  apt.order  <- match.arg(apt.order)
  data       <- .refactorData(data)
  sampleL    <- length(sample.order)
  ret        <- list(matrix = matrix(0)) # placeholder: reserve position 1
  class_tab  <- NA
  groups     <- NULL
  ord        <- seq_len(nrow(data))

  if ( is.null(anno_tbl) ) {
    anno_tbl <- getAnalyteInfo(data)
  }

  if ( !is.null(sample.order) ) {
    if ( sampleL > 2L && is.numeric(sample.order) ) {
      if ( sampleL != nrow(data) ) {
        stop(
          "Incorrect number of row indices: ", value(nrow(data)),
          " rows vs. ", value(sampleL), " indices.", call. = FALSE
        )
      } else {
        data      <- data[sample.order, ]
        ret$y.lab <- "Samples (User Specified Order)"
        ord       <- sample.order
      }

    } else if ( is.character(sample.order) && sampleL %in% 1:2L ) {

      stopifnot(all(sample.order %in% names(data)))
      if ( sampleL == 1L ) {
        ord <- order(data[[ sample.order ]])
      } else if ( sampleL == 2L ) {
        ord <- order(data[[ sample.order[1L] ]], data[[ sample.order[2L] ]])
      }
      data      <- data[ord, ]
      class_tab <- table(data[, sample.order ])
      groups    <- data[, unique(c("SampleId", sample.order)) ]
      ret$y.lab <- sprintf("Samples (by %s)",
                           paste(sample.order, collapse = "*"))
    }
  }

  fold_mat <- apply(data[, getAnalytes(data)], 2, function(.x) log2(.x / median(.x)))
  fold_mat[fold_mat > max]  <- max
  fold_mat[fold_mat < -max] <- -max

  if ( !is.null(threshold) ) {
    if ( threshold > max ) {
      stop(
        "The `max =` argument (", max, ") must be greater than ",
        "`threshold =` argument (", value(threshold), ").", call. = FALSE
      )
    }
    fold_mat[abs(fold_mat) < threshold] <- 0
  }


  # reorder by dilution mix
  if ( is.na(apt.order) ) {
    ret$x.lab <- "Proteins Ordered in Adat"
  } else if ( apt.order == "dilution" ) {

    apt.dils     <- .getDilList(anno_tbl)
    fold_mat     <- fold_mat[, intersect(unlist(apt.dils), colnames(fold_mat)) ]
    ret$dil.nums <- lengths(apt.dils)
    ret$x.lab    <- sprintf("Dilution Ordered Proteins (%s)",
                            paste(names(apt.dils), collapse = " | "))

  } else if ( apt.order == "cluster" ) {
    # this doesn't work quite right, non-square matrix
    Ord <- seriation::get_order(seriation::seriate(dist(fold_mat), method = "OLO"))
    fold_mat  <- fold_mat[Ord, Ord]
    ret$x.lab <- "Clustered Proteins by OLO"
  } else {
    stop("Problem with `apt.order =` argument: ", value(apt.order), call. = FALSE)
  }

  ret$matrix       <- fold_mat
  ret$class.tab    <- class_tab
  ret$groups       <- groups
  ret$title        <- bquote(Fold ~ Change ~ Matrix: ~ italic(log)[2](x / median(x)))
  ret$sample.order <- ord
  ret$legend.sub   <- "Proteins"
  invisible(
    structure(
      ret,
      class = c("foldchange_matrix", "Map", "list")
    )
  )
}


#' @describeIn calcFoldchangeMatrix
#' There is a S3 print method for `"foldchange_matrix"`.
#' @param x An object of class `"foldchange_matrix"`.
#' @param ... Arguments for S3 print methods.
#' @export
print.foldchange_matrix <- function(x, ...) {
  writeLines(
    cli_rule("SomaLogic Fold-Change Matrix", line = "double", line_col = "magenta")
  )
  key <- c(
    "Fold-change matrix dim",
    "Title",
    "x-label",
    "Sample Order",
    "Class Table",
    "Legend Sub-title") |> .pad(25)
  value <- c(
    value(paste(dim(x$matrix), collapse = " x ")),
    value("Fold-Change Matrix: log2(x / median(x))"),
    value(x$x.lab),
    value(x$sample.order),
    c(x$class.tab),
    value(x$legend.sub)
  )
  writeLines(paste(" ", key, value))
  writeLines(cli_rule(line = "double", line_col = "green"))
  invisible(x)
}


#' S3 plot methods for class foldchange_matrix
#' @noRd
#' @export
plot.foldchange_matrix <- function(x, ...) {
  NextMethod("plot", type = "foldchange")
}
