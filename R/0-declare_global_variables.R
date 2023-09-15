# ----------------------------
# Declaring Global Variables:
# This is mostly for passing R CMD checks
# global variables that come from other dependant
# packages, or objects in the data/ directory
# Reference: https://github.com/tidyverse/magrittr/issues/29
# ----------------------------------------------------------
if ( getRversion() >= "2.15.1" )
utils::globalVariables(
  c(".",
    "y",
    "density",
    "type",
    "Group",
    "GroupCount",
    "group",
    "TargetFullName",
    "p.value",
    "Response",
    "AptName",
    "TimePoint",
    "Type",
    "SubjectId",
    "SubjectCount",
    "Sex",
    "group_mean",
    "signed.log2.fold.change",
    ".id",
    "SampleType",
    "RFU_values"
  )
)
