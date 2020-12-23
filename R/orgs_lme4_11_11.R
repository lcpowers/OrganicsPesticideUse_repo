library(tidyverse)
library(haven)
library(plm)
options(scipen = 9999)

rm(list=ls())
data_all = read_dta("./R_input/dta/organicAnalysisPUR copy.dta") 
# Get sample of dataframe to test
data = sample_n(data_all,10000)

data$family[data$family==""] <- NA

fixed <- plm(KgPestAI ~ pur_cdfa_org + lnhectares + soil_quality + lnfarmSz, 
             index = c("year"), 
             data=data, model = "within")
summary(fixed)

random <- plm(KgPestAI ~ pur_cdfa_org + lnhectares + soil_quality + lnfarmSz, 
             index = c("year","permit"), 
             data=data, model = "within")
summary(random)
