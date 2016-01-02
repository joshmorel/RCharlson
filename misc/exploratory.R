library(dplyr)
library(tidyr)
library(ggvis)
setwd("//grh200/Decision Support/REPORT_WORKPRODUCT/2800-2899/2894 - Exploring Advanced Statistical Analysis for Readmissions/Data/outputs")

visits_hig_readm <- read.csv("hig_readmit_output.csv",header=TRUE,stringsAsFactors = FALSE)


#Show the difference between the rates
#When the crude rate is less than the expected rate,
#the risk-adjusted rate is less than the provincial reference rate
#Lower is better for risk-adjusted rate is most important
#For example FY2013 Q2 to Q3 crude rate decreases but risk adjusted stays the same because expected also decreases



visits_hig_readm %>% mutate(fiscal_yr_qtr = paste(fiscal_year,paste("Q",fiscal_qtr,sep=""),sep="-")) %>%
        group_by(fiscal_yr_qtr) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-fiscal_yr_qtr) %>%
        ggvis(~fiscal_yr_qtr,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Fiscal Year and Quarter") %>%
        add_axis("y", title = "Readmission Rate")



visits_hig_readm %>% group_by(agegroupn) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-agegroupn) %>%
        ggvis(~agegroupn,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Age Group (age in years divided by 5 + 1") %>%
        add_axis("y", title = "Readmission Rate")


visits_hig_readm %>% group_by(cohort) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-cohort) %>%
        ggvis(~cohort,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Cohort") %>%
        add_axis("y", title = "Readmission Rate")


visits_hig_readm %>% filter(cohort=="ami") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-hig) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "AMI HIGs") %>%
        add_axis("y", title = "Readmission Rate")

visits_hig_readm %>% filter(cohort=="gi") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-hig) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "GI HIGs") %>%
        add_axis("y", title = "Readmission Rate")


visits_hig_readm %>% filter(cohort=="card") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-hig) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Cardiac HIGs") %>%
        add_axis("y", title = "Readmission Rate")


visits_hig_readm %>% filter(cohort=="pneu") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-hig) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Pneumonia HIGs") %>%
        add_axis("y", title = "Readmission Rate")

visits_hig_readm %>% filter(cohort=="cva") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference,
                  Count = n()
        ) %>%
        gather(key=Ratio,value=Value,-c(Count,hig)) %>%
        mutate(Count=ifelse(Ratio!="Risk.adjusted",NA,Count)) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        layer_points(size=~Count) %>%
        add_axis("x", title = "Stroke HIGs") %>%
        add_axis("y", title = "Readmission Rate") %>%
        add_legend(scales = "stroke", properties = legend_props(legend = list(y = 0))) %>%
        add_legend(scales = "size", properties = legend_props(legend = list(y = 100)))




visits_hig_readm %>% filter(cohort=="gi") %>%
        group_by(hig) %>%
        summarize(Crude=mean(hig_readm_numer),
                  Expected = mean(readm_prob),
                  Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = Crude/Expected*Provincial.Reference,
                  Count = n()
        ) %>%
        gather(key=Ratio,value=Value,-c(Count,hig)) %>%
        mutate(Count=ifelse(Ratio!="Risk.adjusted",NA,Count)) %>%
        ggvis(~hig,~Value,stroke=~Ratio) %>%
        layer_lines() %>%
        layer_points(size=~Count) %>%
        add_axis("x", title = "gi HIGs") %>%
        add_axis("y", title = "Readmission Rate") %>%
        add_legend(scales = "stroke", properties = legend_props(legend = list(y = 0))) %>%
        add_legend(scales = "size", properties = legend_props(legend = list(y = 100)))



setwd("~/../RCharlson/data")
readm_model_params = read.csv("readm_model_params.csv",stringsAsFactors = FALSE)
#Statistically significant per group
subset(readm_model_params,p=="<.0001"|as.numeric(p)<=.05)



glm.fit <- glm(hig_readm_numer ~ agegroupn + fiscal_year + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + cohort,
               family="binomial",data=visits_hig_readm)
library(MASS)
glm.fit.overall <- stepAIC(glm.fit,direction="both")

#Pseudo R-squared, how much of variability in response is explained by explanatory variables

pseudo.r.squared <- function(model) {
        1 - sum((summary(model)$deviance.resid)^2)/summary(model)$null.deviance
}

fit.by.cohort <- function(readm_data,input_cohort) {
        readm_data = subset(readm_data,cohort==input_cohort)
        glm.fit <- glm(hig_readm_numer ~ agegroupn + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + hig,
                       family="binomial",data=readm_data)

        glm.fit.best = stepAIC(glm.fit,direction="both")
}

readm.fit <- function(readm_data) {
        if(length(unique(readm_data$hig)) > 1) {
                glm.fit <- glm(hig_readm_numer ~ agegroupn + fiscal_year + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + hig,
                       family="binomial",data=readm_data)
        }
        else {
                glm.fit <- glm(hig_readm_numer ~ agegroupn + fiscal_year + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender,
                               family="binomial",data=readm_data)
        }

        glm.fit.best = stepAIC(glm.fit,direction="both")
}

#puts model for each cohort into a list
readm.models = lapply(split(visits_hig_readm,visits_hig_readm$cohort),readm.fit)

round(sapply(readm.models,pseudo.r.squared),4)

#For Readmission Rate - Those with Risk Adjusted
visits_hig_readm %>% group_by(prev_admit_index=as.factor(prev_admit_index)) %>%
        summarize(crude_readm_rate=mean(hig_readm_numer),
                  expected_readm_rate = mean(readm_prob),
                  prov_ref_rate=mean(provincial_reference_rate),
                  risk_adjusted_readm_rate = crude_readm_rate/expected_readm_rate*prov_ref_rate
        ) %>%
        melt(id.vars="prev_admit_index") %>%
        ggvis(~prev_admit_index,~value,stroke=~variable) %>%
        layer_lines() %>%
        add_axis("x", title = "Previous Admit Index") %>%
        add_axis("y", title = "Readmission Rate")
