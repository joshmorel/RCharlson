

setwd("C:/Users/Josh.Josh-PC/Documents/healthcarerisk/test")
library(testthat)
library(healthcarerisk)

abstract1_expected <- read.csv("abstract1_expected.csv",header=TRUE,stringsAsFactors=FALSE)
abstract2_expected <- read.csv("abstract2_expected.csv",header=TRUE,stringsAsFactors=FALSE)
abstract3_expected <- read.csv("abstract3_expected.csv",header=TRUE,stringsAsFactors=FALSE)

expect_equal(charlsonindex(abstract1_expected[,1:51],diag_types=TRUE)$charlson_index,abstract1_expected$charlson_index)
expect_equal(charlsonindex(abstract2_expected[,1:23],diag_types=TRUE)$charlson_index,abstract2_expected$charlson_index)
expect_equal(charlsonindex(abstract3_expected[,1:26])$charlson_index,abstract3_expected$charlson_index)



# if(charlson_index_total == 0) {
#         comorbidity_index = 0
# } else if (charlson_index_total %in% c(1,2)) {
#         comorbidity_index = 1
# } else {comorbidity_index = 2}





#Column index of first diagnosis code
diag_index = grep("diag_code_01",colnames(visits))

actual_columns = colnames(visits)[diag_index:(diag_index+49)]

if (mean(actual_columns == actual_columns) < 1 | is.na(mean(actual_columns == actual_columns))) {
        stop("diag_code_01 to diag_code_25 and diag_type_01 to diag_type_25 are required in that order")}


setwd("C:/Users/Josh.Josh-PC/Documents/healthcarerisk/test")
visits = read.csv("sourcedata_for_charlson.csv",header=TRUE,stringsAsFactors = FALSE)

