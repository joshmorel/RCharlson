library(dplyr)
library(tidyr)
library(ggvis)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(stringr)
setwd("//grh200/Decision Support/REPORT_WORKPRODUCT/2800-2899/2894 - Exploring Advanced Statistical Analysis for Readmissions/Data/outputs")

visits_hig_readm <- read.csv("hig_readmit_output.csv",header=TRUE,stringsAsFactors = FALSE)
visits_hig_readm$discharge_date <- with(visits_hig_readm,as.Date(discharge_date,format='%Y-%m-%d'))
visits_hig_readm$fiscal_yr_qtr <- with(visits_hig_readm,paste(fiscal_year,paste("Q",fiscal_qtr,sep=""),sep="-"))

visits_hig_readm$fiscal_mt <- with(visits_hig_readm,str_sub(paste("0",as.character(ifelse(month(discharge_date) <= 3, month(discharge_date) + 9, month(discharge_date) - 3)),sep=""),-2,-1))
visits_hig_readm$fiscal_yr_mt <- with(visits_hig_readm,paste(fiscal_year,fiscal_mt,
                                            as.character(visits_hig_readm$discharge_date,format="%b"),sep="-"))


visits_hig_readm_summary <- read.csv("hig_readmit_output_summary.csv",header=TRUE,stringsAsFactors = FALSE)

summarize_readmissions <- function(detailed_data,key = "fiscal_year") {
        require(dplyr)

        dots = sapply(key, . %>% {as.formula(paste0('~', .))})


        summarized <- detailed_data %>% group_by_(.dots = dots) %>%
                summarize(prov_ref_rate=mean(provincial_reference_rate),
                          expected_readm_rate = mean(readm_prob),
                          readmissions = sum(hig_readm_numer),
                          indexcases = n()
                          ) %>%
                mutate(
                        crude_rate = readmissions/indexcases,
                          risk_adjusted_readmissions = readmissions/expected_readm_rate*prov_ref_rate,
                          cilower_readmissions = prov_ref_rate*(readmissions - 1.96*sqrt(expected_readm_rate*(1-expected_readm_rate)))/expected_readm_rate,
                        cilower_readmissions = ifelse(cilower_readmissions<0,0,cilower_readmissions),
                          ciupper_readmissions = prov_ref_rate*(readmissions + 1.96*sqrt(expected_readm_rate*(1-expected_readm_rate)))/expected_readm_rate,
                          risk_adjusted_rate = risk_adjusted_readmissions/indexcases,
                          cilower_readmission_rate = cilower_readmissions/indexcases,
                          ciupper_readmission_rate = ciupper_readmissions/indexcases
                )
        return(summarized)
}

visits_hig_readm_by_year <- summarize_readmissions(visits_hig_readm)





visits_hig_readm_by_fiscal_yr_qtr <- summarize_readmissions(visits_hig_readm,"fiscal_yr_qtr")
visits_hig_readm_by_prev_admit_index <- summarize_readmissions(visits_hig_readm,"prev_admit_index")



#Shows performance vs Provincial Reference Rate.
#If CI is completely below provincial reference rate we are performing better than expected
#If CI is complete above provincial reference rate we are performing worse than expected
#If CI crosses reference rate we are performing the same as expected
#Expected based on case-mix, comorbidities, demographics and past admission history

# By Quarter

cols2 <- c("Crude"="#00BFC4","Expected"="#F8766D")
g2 <- ggplot(visits_hig_readm_by_fiscal_yr_qtr,aes(x=fiscal_yr_qtr,y=crude_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="Crude")) +
        geom_line(aes(y=expected_readm_rate,group=1,color="Expected"),linetype = 2) +
        labs(x = "Fiscal Year & Quarter", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols2) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Crude vs Expected Readmission Rates for Select HIGs by Quarter")

cols1 <- c("RiskAdjusted"="#00BFC4","ProvincialReference"="#F8766D")
g1 <- ggplot(visits_hig_readm_by_fiscal_yr_qtr,aes(x=fiscal_yr_qtr,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Quarter", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                    values =cols1) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Risk-Adjusted Readmissions for Select HIGs by Quarter")

grid.arrange(g2,g1)

#By Month
visits_hig_readm_by_fiscal_yr_mt <- summarize_readmissions(visits_hig_readm,"fiscal_yr_mt")

g3 <- ggplot(visits_hig_readm_by_fiscal_yr_mt,aes(x=fiscal_yr_mt,y=crude_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="Crude")) +
        geom_line(aes(y=expected_readm_rate,group=1,color="Expected"),linetype = 2) +
        labs(x = "Fiscal Year & Month", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols2) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Crude vs Expected Readmission Rates for Select HIGs by Month") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

g4 <- ggplot(visits_hig_readm_by_fiscal_yr_mt,aes(x=fiscal_yr_mt,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Month", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Risk-Adjusted Readmissions for Select HIGs by Month") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g3,g4)


#By Cohort
visits_hig_readm_by_cohort <- summarize_readmissions(visits_hig_readm,"cohort")

g5 <- ggplot(visits_hig_readm_by_cohort,aes(x=cohort,y=crude_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="Crude")) +
        geom_line(aes(y=expected_readm_rate,group=1,color="Expected"),linetype = 2) +
        labs(x = "Cohort", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols2) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Crude vs Expected Readmission Rates for Select HIGs by Cohort")

g6 <- ggplot(visits_hig_readm_by_cohort,aes(x=cohort,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Cohort", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Risk-Adjusted Readmissions for Select HIGs by Cohort")

grid.arrange(g5,g6)


#By Program

visits_hig_readm_by_program <- summarize_readmissions(visits_hig_readm,"program")

g7 <- ggplot(visits_hig_readm_by_program,aes(x=program,y=crude_rate)) +
        geom_point(aes(size=indexcases),color="#00BFC4") +
        geom_line(aes(group=1,color="Crude")) +
        geom_line(aes(y=expected_readm_rate,group=1,color="Expected"),linetype = 2) +
        labs(x = "Program", y = "Ratio", size = "Index Cases") +
        scale_colour_manual(name = 'Ratio',
                            values =cols2) +
        theme_bw() +
        theme(legend.position="top",legend.box = "horizontal") +
        ggtitle("GRH Crude vs Expected Readmission Rates for Select HIGs by Program")

g8 <- ggplot(visits_hig_readm_by_program,aes(x=program,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Cohort", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="top") +
        ggtitle("GRH Risk-Adjusted Readmissions for Select HIGs by Program")

grid.arrange(g7,g8)



#by program by quarter

visits_hig_readm_by_prog_qtr <- summarize_readmissions(visits_hig_readm,c("fiscal_yr_qtr","program"))
visits_hig_readm_by_prog_qtr <- filter(visits_hig_readm_by_prog_qtr,!(program %in% c("NICU","Childbirth")))

#Issues by Program
ggplot(visits_hig_readm_by_prog_qtr,aes(x=fiscal_yr_qtr,y=risk_adjusted_rate)) +
        geom_point(aes(shape=program),color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Quarter", y = "Ratio") +
        scale_colour_manual(name = 'Ratio',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="top") +
        facet_grid(program ~  .) +
        ggtitle("GRH Risk-Adjusted Readmissions for Select HIGs by Quarter")


#By Program by Month - high volume vs low volume

visits_hig_readm_by_prog_mt <- summarize_readmissions(visits_hig_readm,c("fiscal_yr_mt","program"))
visits_hig_readm_by_prog_mt <- filter(visits_hig_readm_by_prog_mt,!(program %in% c("NICU","Childbirth")))


#Difference between high/med/low volume in terms of error bars
g9 <- ggplot(subset(visits_hig_readm_by_prog_mt,program=="Medicine"),aes(x=fiscal_yr_mt,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Month - Stroke", y = "Ratio") +
        scale_colour_manual(name = 'High Vol - Medicine',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="right") +
        theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

g10 <- ggplot(subset(visits_hig_readm_by_prog_mt,program=="Stroke"),aes(x=fiscal_yr_mt,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Month - Stroke", y = "Ratio") +
        scale_colour_manual(name = 'Medium Vol - Stroke',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="right") +
        theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

g11 <- ggplot(subset(visits_hig_readm_by_prog_mt,program=="Critical Care"),aes(x=fiscal_yr_mt,y=risk_adjusted_rate)) +
        geom_point(color="#00BFC4") +
        geom_line(aes(group=1,color="RiskAdjusted")) +
        geom_line(aes(y=prov_ref_rate,group=1,color="ProvincialReference"),linetype = 2) +
        geom_errorbar(aes(ymin = cilower_readmission_rate,ymax=ciupper_readmission_rate),
                      color ="#00BFC4",width=.2) +
        labs(x = "Fiscal Year & Month - Stroke", y = "Ratio") +
        scale_colour_manual(name = 'Low Vol - Critical Care',
                            values =cols1) +
        theme_bw() +
        theme(legend.position="right") +
        theme(axis.title.x = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g9,g10,g11)


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


#Risk-Adjusted and provincial reference are the most useful to pay attention to
#If risk-adjusted is below provincial reference we are performing better
#than expected for the demographics, case mix and complexity of the index patients
#If above we are performing worse than expected given the demographics, case mix and complexity of the patients we treated who qualify as index cases

visits_hig_readm %>% mutate(fiscal_yr_qtr = paste(fiscal_year,paste("Q",fiscal_qtr,sep=""),sep="-")) %>%
        group_by(fiscal_yr_qtr) %>%
        summarize(Provincial.Reference=mean(provincial_reference_rate),
                  Risk.adjusted = mean(hig_readm_numer)/mean(readm_prob)*Provincial.Reference
        ) %>%
        gather(key=Ratio,value=Value,-fiscal_yr_qtr) %>%
        mutate(Ratio=relevel(Ratio,"Risk.adjusted")) %>%
        ggvis(~fiscal_yr_qtr,~Value,stroke=~Ratio,strokeDash=~Ratio) %>%
        layer_lines() %>%
        add_axis("x", title = "Fiscal Year and Quarter") %>%
        add_axis("y", title = "Readmission Ratio") %>%
        scale_nominal("stroke",range=c("blue","green"))


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
