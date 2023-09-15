#' Saves a Figure (Plot) to File
#'
#' A wrapper for [png()], [pdf()], or [jpeg()] to save plots to
#' disk. If a file path is passed to [figure()], it
#' opens a plotting device based on the file extension,
#' passing the same file name to [close_figure()].
#' If `file = NULL`, output is directed to the default plotting device.
#'
#' The [figure()] and [close_figure()] functions
#' are most useful when used inside of another function that creates a plot.
#' By adding a `file =` pass-through argument to a function that creates a plot,
#' the user can toggle between plotting to file or to a graphics device.
#' Supported plotting devices:
#'   \itemize{
#'     \item [png()]
#'     \item [pdf()]
#'     \item [jpeg()]
#'     \item [postscript()] (`*.eps`)
#'   }
#'
#' @family base R
#' @param file Character. The path of the output file passed to [png()],
#'   [pdf()], or [jpeg()]. Plot type determined by file extension.
#' @param height Double. The height of the plot in pixels.
#' @param width Double. The width of the plot in pixels.
#' @param scale A re-scaling of the output to resize window better.
#' @param ... Additional arguments passed to [png()], [pdf()], or [jpeg()].
#' @note The `fontsize` of the plots are constant. If you would like to
#'   increase the font size relative to the plot, you can decrease the plot size.
#'   Alternatively, you can pass `pointsize` as an additional argument.
#' @author Stu Field
#' @return The `file` argument, invisibly.
#' @seealso [png()], [pdf()], [dev.off()]
#' @examples
#' # Create enclosing plotting function
#' createPlot <- function(file = NULL) {
#'   figure(file = file)
#'   on.exit(close_figure(file = file))
#'   plot_data <- withr::with_seed(1, matrix(rnorm(30), ncol = 2))
#'   plot(as.data.frame(plot_data), col = unlist(soma_colors), pch = 19, cex = 2)
#' }
#'
#' # default; no file saved
#' createPlot()
#'
#' if ( interactive() ) {
#'   # Save as *.pdf
#'   createPlot("example.pdf")
#'
#'   # Save as *.png
#'   createPlot("example.png")
#' }
#' @importFrom grDevices pdf png jpeg postscript
#' @export
figure <- function(file, height = 480, width = 480, scale = 1, ...) {
  if ( !is.null(file) ) {
    ext <- file_ext(file)
    if ( isTRUE(ext == "pdf") ) {
      pdf(file = file,
          height = (height / 96) * scale,  # assume 96 px / in
          width = (width / 96) * scale,
          useDingbats = FALSE,
          title = basename(file), ...)
    } else if ( isTRUE(ext == "png") ) {
      png(filename = file,
          height = height * scale,
          width = width * scale, ...)
    } else if ( isTRUE(ext == "eps") ) {
      postscript(file = file,
                 height = height * scale * 100,
                 width = width * scale * 100,
                 horizontal = FALSE,
                 onefile = FALSE,
                 paper = "special", ...)
    } else if ( isTRUE(ext == "jpeg") ) {
      jpeg(filename = file, height = height * scale, width = width * scale, ...)
    } else {
      stop(
        "Could not find file extension ", value(ext),
        " in provided file path: ", value(file), call. = FALSE
      )
    }
  }
  invisible(file)
}


#' @describeIn figure
#' Closes the currently active plotting device with a
#'   [dev.off()] call if a file name is passed. If
#'   `file = NULL`, nothing happens. This function is typically used in
#'   conjunction with [figure()] inside the enclosing function. See example.
#' @export
close_figure <- function(file) {
  if ( !is.null(file) ) {
    grDevices::dev.off()
  }
  invisible(file)
}
