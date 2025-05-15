# Setup ------
data <- SomaDataIO::example_data |> dplyr::filter(SampleType == "Sample")
apts <- getAnalytes(data)
has_rn <- SomaDataIO::has_rn

# missingness map object
meta <- data[, getMeta(data)]
# random assign NAs to df
cols <- rep(seq_len(ncol(meta)), each = 3L)
rows <- withr::with_seed(1,
  as.integer(replicate(ncol(meta), sample(seq_len(nrow(meta)), 3L)))
)
meta[cbind(rows, cols)] <- NA
mm <- calcMissingnessMap(meta)


# Testing ------
test_that("`getFlaggedIds()` returns error if `x` is not the required class", {
  expect_error(
    getFlaggedIds(1:3),
    "Input `x` object must be class `missingness_map`!"
  )
  expect_error(
    getFlaggedIds("foo"),
    "Input `x` object must be class `missingness_map`!"
  )
  expect_error(
    getFlaggedIds(data.frame(x = 1)),
    "Input `x` object must be class `missingness_map`!"
  )
})

test_that("`getFlaggedIds()` returns error if `data` is not a df", {
  expect_error(
    getFlaggedIds(mm, data = 1:3),
    "The `data` argument must be a `data.frame` object."
  )
})

test_that("`getFlaggedIds()` returns error if flags arg is not in [0, 1]", {
  expect_error(
    getFlaggedIds(mm, flags = 1.1),
    "`flags =` argument must be between 0 and 1!"
  )
  expect_error(
    getFlaggedIds(mm, flags = -0.1),
    "`flags =` argument must be between 0 and 1!"
  )
})

test_that("`getFlaggedIds()` trips error if `include` not in `data`", {
  expect_error(
    getFlaggedIds(mm, 0.05, data, "foo"),
    "All `include` must be in `data`."
  )
})

test_that("`getFlaggedIds()` returns 0 row df and msg if no obs are flagged", {
  expect_message(flagged <- getFlaggedIds(mm, 0.8),
                 "No observations were flagged at this flagging proportion:")

  expect_s3_class(flagged, "data.frame")
  expect_false(has_rn(flagged))
  expect_equal(flagged, data.frame(idx = numeric(0)))
})

test_that("`getFlaggedIds()` `include` is ignored `data = NULL`", {
  expect_equal(
    getFlaggedIds(mm),
    getFlaggedIds(mm, data = NULL, include = "SampleId")  # test if `data = NULL`
  )
})


# MissingnessMaps ----
test_that("`getFlaggedIds()` works on `calcMissingnessMap` object using default
          flags = 0.05, no included variables", {
  missing <- getFlaggedIds(mm)
  expect_s3_class(missing, "data.frame")
  expect_false(has_rn(missing))
  expect_equal(head(missing), data.frame(idx = 1:6))
})

test_that("`getFlaggedIds()` works on `calcMissingnessMap` object using
          flags = 0.25, one included variable", {
  missing <- getFlaggedIds(mm, 0.25, meta, "SampleId")
  expect_s3_class(missing, "data.frame")
  expect_false(has_rn(missing))
  expect_equal(head(missing),
               data.frame(idx      = 1:6,
                          SampleId = c("1", "2", "3", "4", "5", "6")))
})

test_that("`getFlaggedIds()` works on `calcMissingnessMap` object using
          flags = 0.3, two included variables", {
  missing <- getFlaggedIds(mm, 0.3, meta, c("SampleId", "Subarray"))
  expect_s3_class(missing, "data.frame")
  expect_false(has_rn(missing))
  expect_equal(head(missing),
               data.frame(idx        = 1:6,
                          SampleId   = c("1", "2", "3", "4", "5", "6"),
                          Subarray   = c(3, 7, 8, 4, 4, 8)))
})
