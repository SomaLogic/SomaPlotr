# Setup ------
data <- SomaDataIO::example_data |> dplyr::filter(SampleType == "Sample")
apts <- getAnalytes(data)
has_rn <- SomaDataIO::has_rn

# outlier map object
data[12, apts[1:2750]] <- data[12, apts[1:2700]] * 100 # 2700 apts - 52%
data[18, apts[1:423]]  <- data[18, apts[1:423]] * 100  # 423 apts ~ 8%
data[20, apts[1:212]]  <- data[20, apts[1:50]] * 100   # 212 apts ~ 4%
om <- calcOutlierMap(data)

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
    "Input `x` object must be class `outlier_map` or `missingness_map`!"
  )
  expect_error(
    getFlaggedIds("foo"),
    "Input `x` object must be class `outlier_map` or `missingness_map`!"
  )
  expect_error(
    getFlaggedIds(data.frame(x = 1)),
    "Input `x` object must be class `outlier_map` or `missingness_map`!"
  )
})

test_that("`getFlaggedIds()` returns error if `data` is not a df", {
  expect_error(
    getFlaggedIds(om, data = 1:3),
    "The `data` argument must be a `data.frame` object."
  )
})

test_that("`getFlaggedIds()` returns error if flags arg is not in [0, 1]", {
  expect_error(
    getFlaggedIds(om, flags = 1.1),
    "`flags =` argument must be between 0 and 1!"
  )
  expect_error(
    getFlaggedIds(om, flags = -0.1),
    "`flags =` argument must be between 0 and 1!"
  )
})

test_that("`getFlaggedIds()` trips error if `include` not in `data`", {
  expect_error(
    getFlaggedIds(om, 0.05, data, "foo"),
    "All `include` must be in `data`."
  )
})

test_that("`getFlaggedIds()` returns 0 row df and msg if no obs are flagged", {
  expect_message(outliers <- getFlaggedIds(om, 0.8),
                 "No observations were flagged at this flagging proportion:")

  expect_s3_class(outliers, "data.frame")
  expect_false(has_rn(outliers))
  expect_equal(outliers, data.frame(idx = numeric(0)))
})

test_that("`getFlaggedIds()` works on `calcOutlierMap` object, using default
          flags = 0.05, no included variables", {
  outliers <- getFlaggedIds(om, data = data)
  expect_s3_class(outliers, "data.frame")
  expect_false(has_rn(outliers))
  expect_equal(outliers, data.frame(idx = c(12, 18)))
})

test_that("`getFlaggedIds()` works on `calcOutlierMap` object, using
          flags = 0.02, one included variable", {
  outliers <- getFlaggedIds(om, 0.02, data, "SampleId")
  expect_s3_class(outliers, "data.frame")
  expect_false(has_rn(outliers))
  expect_equal(outliers, data.frame(idx      = c(12, 13, 18, 20),
                                    SampleId = c("14", "15", "21", "23")))
})

test_that("`getFlaggedIds()` works on `calcOutlierMap` object, using
          flags = 0.1, multiple included variables", {
  outliers <- getFlaggedIds(om, 0.1, data, c("SampleId", "Sex"))
  expect_s3_class(outliers, "data.frame")
  expect_false(has_rn(outliers))
  expect_equal(outliers, data.frame(idx         = 12,
                                    SampleId    = "14",
                                    Sex         = "M"))
})

test_that("`getFlaggedIds()` works on `calcOutlierMap` object with `data = NULL`", {
  # data = NULL, include = NULL
  outliers <- getFlaggedIds(om)
  expect_s3_class(outliers, "data.frame")
  expect_false(has_rn(outliers))
  expect_equal(outliers, data.frame(idx = c(12, 18)))
})

test_that("`getFlaggedIds()` `include` is ignored `data = NULL`", {
  expect_equal(
    getFlaggedIds(om),
    getFlaggedIds(om, data = NULL, include = "SampleId")  # test if `data = NULL`
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
