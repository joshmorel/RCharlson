#Source functions before processing data, note that charlson index comes from Josh's publicly available package

inputs_directory = "//grh200/Decision Support/REPORT_WORKPRODUCT/2800-2899/2894 - Exploring Advanced Statistical Analysis for Readmissions/Data/inputs/"
outputs_directory = "//grh200/Decision Support/REPORT_WORKPRODUCT/2800-2899/2894 - Exploring Advanced Statistical Analysis for Readmissions/Data/outputs/"

if("RCharlson" %in% rownames(installed.packages())) {
        library(RCharlson)
} else {
        require("devtools")
        install_github("kitjosh1050/RCharlson")

}

predict_readmissions <- function(abstract_data,prov_ref_rate,readm_model_params_path) {
        #create parameters data.frame in global space for re-use by secondary function
        readm_model_params <<- read.csv(readm_model_params_path,stringsAsFactors = FALSE)
        expected_cols = c("cohort","hig","age","gender","calendar_year","prev_admit_index","fiscal_qtr","hig_readm_denom")
        if(mean(expected_cols %in% colnames(abstract_data)) < 1) {
                stop("The abstract data must contain the following required columns - cohort,hig,age,gender,calendar_year,prev_admit_index,fiscal_qtr","hig_readm_denom")
        }
        if(!("charlson_index" %in% colnames(abstract_data))) {
                abstract_data = charlsonindex(abstract_data,diag_types=TRUE)
        }
        abstract_data$gender = tolower(abstract_data$gender)
        abstract_data$hig = gsub("^0", "", abstract_data$hig)
        abstract_data$comorb_index = with(abstract_data,ifelse(charlson_index == 0,0,ifelse(charlson_index >= 3,2,1)))
        abstract_data$agegroupn = with(abstract_data,ifelse(as.integer(age)>=85,18,as.integer(as.integer(age)/5)+1))
        in_readmission_subset = abstract_data$hig_readm_denom == 1
        abstract_data_hig = abstract_data[in_readmission_subset,]

        abstract_data <- abstract_data[!in_readmission_subset,]
        non_hig_qualified <- nrow(abstract_data)
        abstract_data <- cbind(abstract_data,
              readm_odds = rep(as.numeric(NA),non_hig_qualified),
              readm_prob = rep(as.numeric(NA),non_hig_qualified),
              provincial_reference_rate = rep(as.numeric(NA),non_hig_qualified)
        )
        abstract_data_hig$readm_odds = apply(abstract_data_hig,1,calc_prediction)
        abstract_data_hig$readm_prob = with(abstract_data_hig,readm_odds/(readm_odds+1))

        abstract_data_hig$provincial_reference_rate = prov_ref_rate

        abstract_data = rbind(abstract_data,abstract_data_hig)
        return(abstract_data)
}

calc_prediction <- function(abstract_row) {
        if(class(abstract_row) %in% c("character","matrix")) {
                abstract_row = as.data.frame(t(abstract_row),stringsAsFactors = FALSE)
        }
        expected_cols = c("cohort","hig","agegroupn","gender","calendar_year","prev_admit_index","comorb_index","fiscal_qtr")
        if(mean(expected_cols %in% colnames(abstract_row)) < 1) {
                stop("The abstract data must contain all parameter columns - cohort,hig,agegroupn,gender,calendar_year,prev_admit_index,comorb_index,fiscal_qtr")
        }
        abstract_ests = subset(readm_model_params,cohort==abstract_row$cohort & parameter=="intercept")$estimate

        numeric_cols = abstract_row[,c("agegroupn","calendar_year")]
        numeric_cols = data.frame(cohort = abstract_row$cohort, column = names(numeric_cols), values = as.numeric(t(numeric_cols)))
        numeric_cols = merge(numeric_cols,readm_model_params,by.x=c("cohort","column"),by.y=c("cohort","parameter"))
        abstract_ests = append(abstract_ests,with(numeric_cols,values*estimate))

        dummy_cols = abstract_row[,c("hig","gender","prev_admit_index","comorb_index","fiscal_qtr")]
        dummy_cols = data.frame(cohort = abstract_row$cohort, column = names(dummy_cols), values = as.character(t(dummy_cols)))
        dummy_cols = merge(dummy_cols,readm_model_params,by.x=c("cohort","column","values"),by.y=c("cohort","parameter","level"))

        abstract_ests = append(abstract_ests,dummy_cols$estimate)
        estimated_readm_odds = exp(sum(abstract_ests))
        return(estimated_readm_odds)
}



#GRH/IDS-specific - visits_all_hig
#1) Load First Input - HIG & Diagnosis data from DWH, during development include only visits that qualify as HIG index visit
setwd(inputs_directory)
visits_hig <- read.csv("hig_readmit_input_dwh_hig_and_dx.csv",header=TRUE,stringsAsFactors = FALSE)

#visits_hig <- subset(visits_hig,hig_readm_denom==1)
#Calculate Charlson Index function using only pre-admit comorbidity or transfer diagnosis types, then removing diag cols
#for workability
visits_hig <- charlsonindex(visits_hig,diag_types=TRUE)
#visits_hig <- visits_hig[,-grep("diag",colnames(visits_hig))]

#2) Load Second Input - GRH Readmissions to Hospital reporting to IDS to identify numerators
visits_ids = read.csv("hig_readmit_input_ids_readmits.csv",header=TRUE,stringsAsFactors = FALSE)
visits_hig_readm <- merge(visits_hig,visits_ids,by.x="ï..cihi_key",by.y="ï..DAD_Link_Field",all.x=TRUE)

#If some missing in IDS then

missing_in_ids <- subset(visits_hig_readm,is.na(DAD_LHIN_Patient_Identifier))
if(nrow(missing_in_ids) > 0) {
        setwd(outputs_directory)
        write.csv(missing_in_ids,"dq_output_missing_in_ids.csv",row.names=FALSE)
        print(paste(nrow(missing_in_ids), "GRH records missing in IDS"))
}

rm(missing_in_ids)
setwd(inputs_directory)

#Using data.table for ease of manipulation
library(data.table)
visits_hig_readm =  data.table(visits_hig_readm)

#If missing in IDS, assume GRH data are valid and if readmit to GRH then flag as readmit, if in IDS then use IDS data
visits_hig_readm[,hig_readm_numer:= ifelse(is.na(DAD_LHIN_Patient_Identifier) & hig_readm_numer_to_grh=="1",1,
                                           ifelse(DAD_Days_To_Next_DAD_Admit <= 30 & Next_DAD_Admit_Category == "U",1,0))
                 ]

#Show crude readmissions rate by fiscal quarter to see progress
visits_hig_readm[,.(readm_count = sum(hig_readm_numer),index_count=.N),by=.(fiscal_year,fiscal_qtr)]



visits_hig_readm[,c("admit_date",
                    "discharge_date") := list(as.Date(admit_date,format="%Y-%m-%d"),
                                               as.Date(discharge_date,format="%Y-%m-%d"))]


setnames(visits_hig_readm,c("ï..cihi_key","DAD_LHIN_Patient_Identifier","DAD_Index_Discharge_Date__PHI_1","DAD_Institution_Type_To_B_Key",
                            "DAD_Next_DAD_Discharge_Next_DAD_DAD_Link_Field","DAD_Re_Admit_Admit_Date__PHI_1","DAD_Re_Admit_Institution_Name1"
                            ,"DAD_Days_To_Next_DAD_Admit","Next_DAD_Admit_Category","Next_DAD_Institution_Type_From_B_Key","Next_DAD_Institution_Name_From"),
                         c("cihi_key","ids_key","index_discharge_date","index_institution_type_to",
                           "next_cihi_key","next_admit_date","next_institution_name",
                           "days_to_next_admit","next_admit_category","next_institution_type_from","next_institution_name_from")
)

visits_hig_readm[,c("index_discharge_date",
                "next_admit_date") := list(as.Date(index_discharge_date,format="%m/%d/%Y"),
                                        as.Date(next_admit_date,format="%m/%d/%Y"))]

#DQ issue cases where next visit has institution from of AT which were not excluded previously
#Need to decide if we want to exclude these
missed_transfer = visits_hig_readm[next_institution_type_from=="AT" & next_admit_date == discharge_date]
if(nrow(missed_transfer) > 0) {
        setwd(outputs_directory)
        write.csv(missed_transfer,"dq_output_transfer_to_not_coded.csv",row.names=FALSE)
        print(paste(nrow(missed_transfer), "GRH records transfer to potentially miscoded"))
}
rm(missed_transfer)
setwd(inputs_directory)

#Show crude readmissions rate by fiscal quarter after excluding transfers DQ issues
#Index case count is now closer to what MOHLTC is reporting
visits_hig_readm = visits_hig_readm[is.na(next_admit_date) |
        !(next_institution_type_from=="AT" & next_admit_date == discharge_date)]

visits_hig_readm[,.(readm_count = sum(hig_readm_numer),index_count=.N),by=.(fiscal_year,fiscal_qtr)]


#3) Get previous admit data from IDS in order to properly calculate prev_admit_index
visits_ids_prev_admit <- data.table(read.csv("hig_readmit_input_ids_prev_admits.csv",header=TRUE,stringsAsFactors = FALSE))

setnames(visits_ids_prev_admit,c("ï..DAD_Link_Field","DAD_LHIN_Patient_Identifier","DAD_Institution_Name","DAD_Admit_Date__PHI_",
                                 "DAD_Discharge_Date__PHI_","DAD_Discharge_Time__PHI_","DAD_Institution_Type_To_B_Key","DAD_Institution_Name_To"),
                                c("prev_admit_cihi_key","ids_key","prev_admit_institution_name","prev_admit_admit_date"
                                  ,"prev_admit_discharge_date","prev_admit_discharge_time","prev_admit_institution_to_type","prev_admit_instution_to_name")
)

visits_ids_prev_admit[,c("prev_admit_admit_date",
                    "prev_admit_discharge_date") := list(as.Date(prev_admit_admit_date,format="%m/%d/%Y"),
                                                as.Date(prev_admit_discharge_date,format="%m/%d/%Y"))]


#Merge data sets to calculate prev_admit_index
setkey(visits_ids_prev_admit,ids_key)
setkey(visits_hig_readm,ids_key)

visits_prev_admits =  visits_ids_prev_admit[visits_hig_readm
                      ][prev_admit_cihi_key != cihi_key & prev_admit_discharge_date <= discharge_date]


#Transfer to acute treatment within 1 day are not counted for previous admit index
visits_prev_admits = visits_prev_admits[admit_date - prev_admit_discharge_date <= 90
                                        ][!(prev_admit_institution_to_type == "AT" & (admit_date - prev_admit_discharge_date <= 1))]

visits_prev_admits = visits_prev_admits[,.(days_from_prev_admit = as.integer(min(admit_date-prev_admit_discharge_date))),
                                        by=cihi_key
                                        ][,.(cihi_key,
                                             prev_admit_index=ifelse(days_from_prev_admit>60,1,
                                                                    ifelse(days_from_prev_admit>30,2,3)))]

setkey(visits_prev_admits,cihi_key)
setkey(visits_hig_readm,cihi_key)

visits_hig_readm = visits_prev_admits[visits_hig_readm
                                        ][,prev_admit_index:=ifelse(is.na(prev_admit_index),0,prev_admit_index)]

#Show difference between prev_admit_index (from IDS) i.prev_admit_index (internal)
#Note that transfers to "AT" are not counted as previous admit index so some 1-3s will become 0s
ftable(xtabs(~ prev_admit_index + i.prev_admit_index,data=visits_hig_readm))

#4) Calculated expected readmissions using logistic model provided by HSIMI
visits_hig_readm <- predict_readmissions(visits_hig_readm,
                                         prov_ref_rate = 0.164359254690341,
                                         readm_model_params_path = "hig_readmit_input_model_parameters.csv")

visits_hig_readm[,hig_readm_numer:=as.numeric(hig_readm_numer)]

visits_hig_readm_summary = visits_hig_readm[,.(visits=.N,
                                               readmissions = sum(hig_readm_numer),
                                               crude_readm_rate=mean(hig_readm_numer),
                                               expected_readm_rate = mean(readm_prob),
                                               prov_ref_rate=mean(provincial_reference_rate))
                                            ,by=.(fiscal_year,fiscal_qtr)
                                            ][,.(fiscal_year,fiscal_qtr,visits,readmissions,crude_readm_rate,expected_readm_rate,prov_ref_rate,
                                                 risk_adjusted_readm_rate = crude_readm_rate/expected_readm_rate*prov_ref_rate)]

visits_hig_readm_summary

setwd(outputs_directory)
write.csv(visits_hig_readm_summary,"hig_readmit_output_summary.csv",row.names=FALSE)
write.csv(visits_hig_readm,"hig_readmit_output.csv",row.names=FALSE)

#Required fixes - DONE
#Change HIG for COPD to correct 2014 method -- need to re-fix in WinRecs
#Rename diabetes cohort form 'db' to 'dm'
#Remove commas from hig_desc as affecting save-to results in SQL Server
#Strip leading zeros from HIG code so they merge correctly with readmission parameters
#Needed to add previous admissions to other hospitals from IDS to better calculate expected readmissions rate
#Excluded unflagged transers - those with same patient next admission on discharge date with transfer from = 'AT'


#Still need to check:
#Expected readmission is slightly higher - about .2 percentage points than HDB website. Might be rounding, or could double-check a few things.
#Need to decide if/how to exclude unflagged transfers -
#437d diabetes with multiple conditions missing - wonder why
