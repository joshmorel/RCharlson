library(dplyr)
library(tidyr)
library(ggvis)
setwd("//grh200/Decision Support/REPORT_WORKPRODUCT/2800-2899/2894 - Exploring Advanced Statistical Analysis for Readmissions/Data")

visits_hig_readm <- read.csv("hig_readmit_output.csv",header=TRUE,stringsAsFactors = FALSE)


#Show the difference between the rates
#When the crude rate is less than the expected rate,
#the risk-adjusted rate is less than the provincial reference rate
#Lower is better for risk-adjusted rate is most important
#For example Q1 and Q3 have virtually same performance although Q3 has 10% lower crude rate

visits_hig_readm %>% group_by(fiscal_qtr = as.factor(fiscal_qtr)) %>%
        summarize(crude_readm_rate=mean(hig_readm_numer),
                  expected_readm_rate = mean(readm_prob),
                  prov_ref_rate=mean(provincial_reference_rate),
                  risk_adjusted_readm_rate = crude_readm_rate/expected_readm_rate*prov_ref_rate
                  ) %>%
        melt(id.vars="fiscal_qtr") %>%
        ggvis(~fiscal_qtr,~value,stroke=~variable) %>%
        layer_lines() %>%
        add_axis("x", title = "Fiscal Quarter") %>%
        add_axis("y", title = "Readmission Rate")


setwd("~/../RCharlson/data")
readm_model_params = read.csv("readm_model_params.csv",stringsAsFactors = FALSE)
#Statistically significant per group
subset(readm_model_params,p=="<.0001"|as.numeric(p)<=.05)



glm.fit <- glm(hig_readm_numer ~ agegroupn + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + cohort,
               family="binomial",data=visits_hig_readm)
library(MASS)
glm.fit.overall <- stepAIC(glm.fit,direction="both")

#Pseudo R-squared, how much of variability in response is explained by explanatory variables

pseudo.r.squared <- function(model) {
        1 - sum((summary(model)$deviance.resid)^2)/summary(model)$null.deviance
}

sapply(readm.models,pseudo.r.squared)

fit.by.cohort <- function(readm_data,input_cohort) {
        readm_data = subset(readm_data,cohort==input_cohort)
        glm.fit <- glm(hig_readm_numer ~ agegroupn + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + hig,
                       family="binomial",data=readm_data)

        glm.fit.best = stepAIC(glm.fit,direction="both")
}

readm.fit <- function(readm_data) {
        if(length(unique(readm_data$hig)) > 1) {
                glm.fit <- glm(hig_readm_numer ~ agegroupn + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender + hig,
                       family="binomial",data=readm_data)
        }
        else {
                glm.fit <- glm(hig_readm_numer ~ agegroupn + factor(comorb_index) + factor(fiscal_qtr) + factor(prev_admit_index) + gender,
                               family="binomial",data=readm_data)
        }

        glm.fit.best = stepAIC(glm.fit,direction="both")
}

#puts model for each cohort into a list
readm.models = lapply(split(visits_hig_readm,visits_hig_readm$cohort),readm.fit)

sapply(readm.models,pseudo.r.squared)
