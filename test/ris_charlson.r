#As suggested by Quan et al we have certain conditions that are mutually exclusivel
#They should not be counted together - malignancy & solid tumour, only the max of the two should be taken.

ris_charlsonindex <- function(abstract_data,diag_types=FALSE) {
        expected_diag_code_cols <- c('diag_code_01','diag_code_02','diag_code_03','diag_code_04','diag_code_05', 'diag_code_06', 'diag_code_07', 'diag_code_08', 'diag_code_09', 'diag_code_10', 'diag_code_11', 'diag_code_12', 'diag_code_13', 'diag_code_14', 'diag_code_15', 'diag_code_16', 'diag_code_17', 'diag_code_18', 'diag_code_19', 'diag_code_20', 'diag_code_21', 'diag_code_22', 'diag_code_23', 'diag_code_24', 'diag_code_25')
        actual_diag_code_cols <- grep("diag_code",colnames(abstract_data),value=TRUE)

        if(mean(expected_diag_code_cols[1:length(actual_diag_code_cols)] == actual_diag_code_cols) < 1) {
                stop("at least diag_code_01, up to diag_code_25 required in that order")
        }

        if(diag_types) {
                expected_diag_type_cols <- c('diag_type_01', 'diag_type_02', 'diag_type_03', 'diag_type_04', 'diag_type_05', 'diag_type_06', 'diag_type_07', 'diag_type_08', 'diag_type_09', 'diag_type_10', 'diag_type_11', 'diag_type_12', 'diag_type_13', 'diag_type_14', 'diag_type_15', 'diag_type_16', 'diag_type_17', 'diag_type_18', 'diag_type_19', 'diag_type_20', 'diag_type_21', 'diag_type_22', 'diag_type_23', 'diag_type_24', 'diag_type_25')
                actual_diag_type_cols <- grep("diag_type",colnames(abstract_data),value=TRUE)
                if(mean(expected_diag_type_cols[1:length(actual_diag_type_cols)] == actual_diag_type_cols) < 1) {
                        stop("if diag_types=TRUE, at least diag_type_01, up to diag_type_25 required in that order")
                }
                if(mean(substr(actual_diag_code_cols, 11, 12) == substr(actual_diag_type_cols, 11, 12)) < 1) {
                        stop("if diag_types=TRUE, number, order and suffixing of diag_type columns must match diag_code columns")
                }
        }

        abstract_data <- as.data.frame(sapply(abstract_data,trimws),stringsAsFactors = FALSE)
        abstract_data <- as.data.frame(sapply(abstract_data,function(x) gsub("\\.|NULL","",x)),stringsAsFactors=FALSE)
        abstract_data <- as.data.frame(sapply(abstract_data,function(x) ifelse(x=="",NA,x)),stringsAsFactors=FALSE)

        charlson = apply(abstract_data,1,function(x) ris_calc_charlsonindex(x,diag_types=diag_types))
        abstract_data$charlson_index = charlson
        return(abstract_data)
}

ris_calc_charlsonindex <- function(diag_row,diag_types) {

        diag_codes = toupper(diag_row[grepl("diag_code",names(diag_row))])

        if(diag_types) {
                diag_types = toupper(diag_row[grepl("diag_type",names(diag_row))])

                if(sum(is.na(diag_codes[!is.na(diag_types)])) > 0) {
                        stop(paste("Diagnosis type without corresponding diagnosis code for row ",diag_row[1],collapse=""))
                }

                if(sum(is.na(diag_types[!is.na(diag_codes)])) > 0) {
                        stop(paste("Diagnosis type without corresponding diagnosis code for row ",diag_row[1],collapse=""))
                }
                comorbid_diag_codes = diag_codes[diag_types %in% c('1','W','Y','X')]
        }
        else {
                comorbid_diag_codes = diag_codes
        }


        if(length(comorbid_diag_codes) == 0) {
                charlson_index_total = 0
                comorbidity_index = 0
        }
        else {
                chf = "I43|I50|I099|I255|I420|I425|I426|I427|I428|I429|P290"
                dementia = "F00|F01|F02|F03|G30|F051|G311"
                cpd = "J40|J41|J42|J43|J44|J45|J47|J60|J61|J62|J63|J64|J65|J66|J67|I278|I279|J684|J701|J703"
                rheum = "M05|M32|M33|M34|M06|M315|M351|M353|M360"
                mild_liver = "B18|K73|K74|K700|K701|K702|K703|K709|K717|K713|K714|K715|K760|K762|K763|K764|K768|K769|Z944"
                comp_diab = "E102|E103|E104|E105|E107|E112|E113|E114|E115|E117|E132|E133|E134|E135|E137|E142|E143|E144|E145|E147"
                hemiplagia = "G81|G82|G041|G114|G801|G802|G830|G831|G832|G833|G834|G839"
                renal = "N18|N19|N052|N053|N054|N055|N056|N057|N250|N032|N033|N034|N035|N036|N037|Z490|Z491|Z492|Z940|Z992"
                malignancy = "C00|C01|C02|C03|C04|C05|C06|C07|C08|C09|C10|C11|C12|C13|C14|C15|C16|C17|C18|C19|C20|C21|C22|C23|C24|C25|C26|C30|C31|C32|C33|C34|C37|C38|C39|C40|C41|C43|C45|C46|C47|C48|C49C50|C51|C52|C53|C54|C55|C56|C57|C58|C60|C61|C62|C63|C64|C65|C66|C67|C68|C69|C70|C71|C72|C73|C74|C75|C76|C81|C82|C83|C84|C85|C88|C90|C91|C92|C93|C94|C95|C96|C97"
                liver = "K704|K711|K721|K729|K765|K766|K767|I850|I859|I864|I982"
                tumour = "C77|C78|C79|C80"
                aids = "B24|O987"
                chf_wt = max(grepl(chf,comorbid_diag_codes)*2)
                dementia_wt = max(grepl(dementia,comorbid_diag_codes)*2)
                cpd_wt = max(grepl(cpd,comorbid_diag_codes)*1)
                rheum_wt = max(grepl(rheum,comorbid_diag_codes)*1)
                mild_liver_wt = max(grepl(mild_liver,comorbid_diag_codes)*2)
                comp_diab_wt = max(grepl(comp_diab,comorbid_diag_codes)*1)
                hemiplagia_wt =  max(grepl(hemiplagia,comorbid_diag_codes)*2)
                renal_wt = max(grepl(renal,comorbid_diag_codes)*1)
                malignancy_wt = max(grepl(malignancy,comorbid_diag_codes)*2)
                liver_wt = max(grepl(liver,comorbid_diag_codes)*4)
                tumour_wt = max(grepl(tumour,comorbid_diag_codes)*6)
                aids_wt = max(grepl(aids,comorbid_diag_codes)*4)
                charlson_index_total = liver_wt + mild_liver_wt + malignancy_wt + tumour_wt + chf_wt + dementia_wt + cpd_wt + rheum_wt + comp_diab_wt + hemiplagia_wt + renal_wt + aids_wt
                #commented out is the correct algorithm as per Quan et al. But It seems RIS is not using that.
                #charlson_index_total = max(liver_wt,mild_liver_wt) + max(malignancy_wt,tumour_wt) + chf_wt + dementia_wt + cpd_wt + rheum_wt + comp_diab_wt + hemiplagia_wt + renal_wt + aids_wt
        }
        return(charlson_index_total)
}
