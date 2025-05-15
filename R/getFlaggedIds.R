#' Get Flagged Ids From MAD Outlier or Missingness Maps
#'
#' Return the IDs of flagged samples for objects of the
#' `missingness_map` class. Samples are flagged based on the percent of
#' "missingness" (`NAs`) of clinical meta data of an ADAT.
#'
#' @family Calc Map
#' @inheritParams plot.Map
#' @param x An object of class: `missingness_map` - from [calcMissingnessMap()]
#' @param data Optional. The data originally used to create the map `x`. If
#'   omitted, a single column data frame is returned.
#' @param include Optional. Character vector of column name(s) in `data` to
#'   include in the resulting data frame. Ignored if `data = NULL`.
#' @return A `data.frame` of the indices (`idx`) of flagged samples, along
#'   with any additional variables as specified by `include`.
#' @author Caleb Scheidel
#' @examples
#' # flagged missingness
#' sample.adat <- SomaDataIO::example_data
#' nc   <- SomaDataIO::getMeta(sample.adat, n = TRUE)
#' meta <- sample.adat[, SomaDataIO::getMeta(sample.adat)]
#'
#' # random assign NAs to df b/c currently none missing
#' cols <- rep(1:nc, each = 3L)
#' rows <- withr::with_seed(1, as.integer(replicate(nc, sample(1:nrow(meta), 3))))
#' meta[cbind(rows, cols)] <- NA
#'
#' mm <- calcMissingnessMap(meta)
#' getFlaggedIds(mm, meta, flags = 0.2)
#' @importFrom SomaDataIO rm_rn
#' @export
getFlaggedIds <- function(x, flags = 0.05, data = NULL, include = NULL) {

  if ( !inherits(x, c("missingness_map")) ) {
    stop("Input `x` object must be class `missingness_map`!",
         call. = FALSE)
  }

  # ensure that flags value is between 0 & 1
  if ( flags < 0 || flags > 1 ) {
    stop("`flags =` argument must be between 0 and 1!", call. = FALSE)
  }

  flagged <- which(rowSums(x$matrix) >= ncol(x$matrix) * flags) |> unname()
  df_idx  <- data.frame(idx = flagged)  # default 1-col df

  if ( !length(flagged) ) {
    .info("No observations were flagged at this flagging proportion:",
          value(flags))
  }

  if ( is.null(data) ) {
    df_idx
  } else {
    stopifnot(
      "The `data` argument must be a `data.frame` object." = is.data.frame(data),
      "All `include` must be in `data`." = all(include %in% names(data))
    )
    df <- as.data.frame(data)  # strip soma_adat class
    cbind(
      df_idx,
      rm_rn(df[flagged, include, drop = FALSE])   # ensure no rn
    )
  }
}
