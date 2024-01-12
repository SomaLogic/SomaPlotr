# Inspired by `expect_snapshot_file()` documentation
save_png <- function(code, ..., gg = TRUE) {
  path <- figure(tempfile(fileext = ".png"), ...)
  on.exit(close_figure(path))
  if ( gg ) {
    print(force(code))
  } else {
    force(code)
  }
  path
}

expect_snapshot_plot <- function(code, name, ...) {
  name <- paste0(name, ".png")
  withr::defer(unlink(name, force = TRUE))
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file
  announce_snapshot_file(name = name)
  # only run on MacOS
  skip_on_os(c("linux", "windows"))
  path <- save_png(code, ...)
  expect_snapshot_file(path, name)
}

`%[[%` <- function(x, y) {
  vals <- vapply(x, typeof, "")
  stopifnot(length(unique(vals)) == 1L)
  unlist(lapply(x, `[[`, i = y))
}
