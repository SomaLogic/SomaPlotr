
#' @keywords internal package
"_PACKAGE"


# on load, make all the package objects
# available in the namespace for internal use
.onLoad <- function(...) {
  # the objects passed here must match the *.rda files
  # inside the `data/` directory
  # `plot.rda` contains most of the pkg objects
  utils::data("plotr", package = "SomaPlotr",
              envir = parent.env(environment()))
  invisible()
}
