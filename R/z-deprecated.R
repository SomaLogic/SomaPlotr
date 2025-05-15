#' Deprecated function(s) of the \pkg{SomaPlotr} package
#'
#' @description
#' These functions have either been
#' `r lifecycle::badge("deprecated")`
#' in the current version of \pkg{SomaPlotr} package.
#' Please re-code your scripts accordingly based on the
#' suggestions below:
#'
#' \tabular{lcr}{
#'   **Function**       \tab                                    \tab **Now Use** \cr
#'   [calcOutlierMap()]    \tab `r lifecycle::badge("deprecated")` \tab `SomaDataIO::calcOutlierMap()` \cr
#' }
#'
#' @details
#' Some badges you may see in \pkg{SomaPlotr}:
#'
#' `r lifecycle::badge("superseded")`
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `r lifecycle::badge("stable")`
#'
#' @name SomaPlotr-deprecated
#' @aliases calcOutlierMap
#' @importFrom lifecycle deprecate_stop
NULL

#' @export
calcOutlierMap <- function() {
  deprecate_stop("0.0.1", "SomaPlotr::calcOutlierMap()", "SomaDataIO::calcOutlierMap()")
}
