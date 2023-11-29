
.is_int <- function(x) {
  if ( !is.numeric(x) ) {
    return(FALSE)
  }
  all(floor(x) == x, na.rm = TRUE)
}

.calcCCC <- function(x, y) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  k   <- length(x)
  sdx <- stats::sd(x)
  sdy <- stats::sd(y)
  rho <- stats::cor(x, y, method = "pearson")
  v   <- sdx / sdy
  sx2 <- stats::var(x) * (k - 1) / k
  sy2 <- stats::var(y) * (k - 1) / k
  u  <- (mean(x) - mean(y)) / ((sx2 * sy2)^0.25)
  Cb <- ((v + 1 / v + u^2) / 2)^-1
  pc <- rho * Cb
  sep <- ((1 - rho^2) * pc^2 * (1 - pc^2) / rho^2 +
          (2 * pc^3 * (1 - pc) * u^2 / rho) - 0.5 * pc^4 * u^4 / rho^2) / (k - 2)
  sep <- sqrt(sep)
  Z <- 0.5 * log((1 + pc) / (1 - pc))
  pval <- 2 * stats::pnorm(-abs(Z))
  set <- sep / (1 - pc^2)
  N. <- 1 - (0.05 / 2)
  up <- Z + stats::qnorm(N.) * set
  lo <- Z - stats::qnorm(N.) * set
  lo <- (exp(2 * lo) - 1) / (exp(2 * lo) + 1)
  up <- (exp(2 * up) - 1) / (exp(2 * up) + 1)
  list(rho.c = pc, ci95 = c(lower = lo, upper = up), Z = Z, p.value = pval)
}

.fitGauss <- function(x) {
  x   <- x[!is.na(x)]
  y   <- rank(x, ties.method = "max") / length(x)
  mu  <- stats::median(x)
  sigma <- stats::mad(x, center = mu, constant = 1.4826)
  fit <- stats::nls(
    formula = y ~ stats::pnorm(x, mean = mu, sd = sigma),
    data    = data.frame(x = x, y = y),
    start   = list(mu = mu, sigma = sigma),
    control = stats::nls.control(maxiter = 2000, minFactor = 1/1024, warnOnly = TRUE)
  )
  as.list(stats::coef(fit))
}

#' @importFrom SomaDataIO getMeta
#' @noRd
.refactorData <- function(data) {
  lgl <- vapply(data[getMeta(data)], is.factor, NA, USE.NAMES = TRUE)
  nms <- names(lgl[lgl])
  for ( meta in nms ) {
    levs <- levels(data[[meta]])
    data[[meta]] <- droplevels(data[[meta]])
    sdiff <- setdiff(levs, levels(data[[meta]]))
    if ( length(sdiff) > 0L && interactive() ) {
      .info(
        paste("Dropping levels", value(sdiff), "from", value(meta))
      )
    }
  }
  data
}

is_chr <- function(x) {
  identical(typeof(x), "character") && length(x) == 1L
}

has_length <- function(x) {
  length(x) > 0L
}

file_ext <- function(x) {
  gsub("(.+)([.])([^.]+)$", "\\3", basename(x), perl = TRUE)
}

# friendly version of ifelse
`%||%` <- getFromNamespace("%||%", ns = "SomaDataIO")

# other borrowed functions
.pad <- getFromNamespace(".pad", ns = "SomaDataIO")
cli_rule <- getFromNamespace("cli_rule", ns = "SomaDataIO")

# replacement for SomaDataIO::.value() that does not require usethis::ui_value()
value <- function(x) {
  if ( identical(Sys.getenv("TESTTHAT"), "true") ) {
    paste(encodeString(x, quote = "'"), collapse = ", ")
  } else {
    if (is.character(x)) {
      x <- encodeString(x, quote = "'")
    }
    x <- paste0("\033[34m", x, "\033[39m") # colors console output blue
    paste0(x, collapse = ", ")
  }
}

# workaround hacks for usethis ui_*() functions
.info <- function(...) {
  .inform(ifelse(l10n_info()$`UTF-8`, "\u2139", "i"), ...)
}
.done <- function(...) {
  .inform(ifelse(l10n_info()$`UTF-8`, "\u2713", "v"), ...)
}
.inform <- function(sym, ...) {
  msg <- paste(sym, paste(...), "\n")
  withRestarts(muffleMessage = function() NULL, {
    signalCondition(structure(list(message = msg), class = c("message", "condition")))
    cat(msg, sep = "", file = stdout())
  })
  invisible()
}

# this is a clone of `getAptamerDilution()` from internal source code
# hard-coded to drop-hybs
.getDilList <- function(ad) {
  ad <- dplyr::filter(ad, !grepl("^Hybridization", Type))
  stopifnot("Dilution" %in% names(ad), "AptName" %in% names(ad))
  split(ad$AptName, ad$Dilution)
}

# this is adapted from `getOutliers()` and only retains nonparametric-type
# calculations
#' @importFrom stats mad median
.getOutliers <- function(x, fold.crit = 5) {
  med       <- median(x, na.rm = TRUE)
  stat_bool <- abs( x - med ) > 6 * mad(x, constant = 1) # stat criterion
  fold_bool <- (x / med > fold.crit) | (med / x > fold.crit)    # FC criterion
  which(stat_bool & fold_bool)
}

# Vectors of expected feature columns in a SomaLogic ADAT, split by class
known_chr <- c("PlateId", "SampleId", "SampleType", "SampleMatrix",
               "Barcode2d")
known_dbl <- c("SlideId", "Subarray", "HybControlNormScale")
