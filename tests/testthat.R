# library(testthat)
library(devtools)
setwd("/mnt/iusers01/ja01/p28917yz/developments/valiupdater")
# devtools::document()

# test_dir("tests/testthat")
test(filter = "model")
test(filter = "predict")
test(filter = "validate_plots")
