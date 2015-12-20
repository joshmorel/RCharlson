


#Column index of first diagnosis code
diag_index = grep("diag_code_01",colnames(visits))

actual_columns = colnames(visits)[diag_index:(diag_index+49)]

if (mean(actual_columns == actual_columns) < 1 | is.na(mean(actual_columns == actual_columns))) {
        stop("diag_code_01 to diag_code_25 and diag_type_01 to diag_type_25 are required in that order")}

visits = read.csv("sourcedata_for_charlson.csv",header=TRUE,stringsAsFactors = FALSE)

setwd("C:/Users/Josh.Josh-PC/Documents/DataScience/Packages/healthcarerisk/test")


# if(charlson_index_total == 0) {
#         comorbidity_index = 0
# } else if (charlson_index_total %in% c(1,2)) {
#         comorbidity_index = 1
# } else {comorbidity_index = 2}
