library(tidyverse)
library(haven)
library(pastecs)
library(lme4)
library(robustlmm)
library(gtools)
library(dotwhisker)
library(margins)
library(merDeriv)
options(scipen = 9999)

setwd("~/Desktop/Projects/Organics/organics_repo")



rm(list=ls())
data_all = read_dta("./R_input/dta/organicAnalysisPUR copy.dta")
data_all$permit = as.factor(data_all$permit)

# Get sample of dataframe to test
data = data_all

data$family[data$family==""] <- NA

#### Following along with Ashley's code ####

# Add numeric variable for family
unique_families <- data.frame(family = unique(data$family)) %>% 
  dplyr::mutate(family_id = as.factor(1:nrow(.)))
data = merge(data,unique_families)

#### Will maybe create a variable for permit family: just based on unique permit family groups ####

#### Results Figure 1 ####

# Pesticide use outcomes to loop through
pest_outcomes <- c("PestAI")
i = pest_outcomes[1]

# summary(lm(lnKgPestAIHa100 ~ pur_cdfa_org + + soil_quality + lnfarmSz, data = data))

for(i in pest_outcomes){

  print(i)

  # Estimate first hurdle with probit
  h1_outcome = paste0("Kg",i,"Ha100")

  # Make binary outcome variable -- glmer does not do that automatically
  h1_data = dplyr::mutate(data, binary_outcome = ifelse(eval(as.name(h1_outcome))>0,1,0))

  #### Mixed effect probit for hurdle 1 - spray or no spray ####
  timestamp()
  # me_h1 <- glmer(binary_outcome ~ pur_cdfa_org + lnhectares + soil_quality + lnfarmSz +
  #                                 (1|family_id) + (1|permit),
  #                   family = binomial(link = "probit"),
  #                   data = h1_data)
  
  me_h2_fid <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id), 
                     family = binomial(link = "probit"),
                     data = h1_data)
  me_h2_perm <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|permit), 
                      family = binomial(link = "probit"),
                      data = h1_data)
  me_h2_year <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|year), 
                      family = binomial(link = "probit"),
                      data = h1_data)
  
  me_h2_fidyr <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|year),
                       family = binomial(link = "probit"),
                       data = h1_data)
  me_h2_fidperm <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|permit),
                         family = binomial(link = "probit"),
                         data = h1_data)
  me_h2_yrperm <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|permit) + (1|year),
                        family = binomial(link = "probit"),
                        data = h1_data)
  
  me_h2_all <- glmer(binary_outcome ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|permit) + (1|year),
                     family = binomial(link = "probit"),
                     data = h1_data)
  
  anova(me_h2_fid ,me_h2_perm, me_h2_year, me_h2_fidyr, me_h2_fidperm, me_h2_yrperm, me_h2_all)
  
  summary(me_h1)
  bread.lmerMod(me_h1,full = T)
  assign(paste0("glm_",h1_outcome), me_h1)

  margeffects = summary(margins(me_h1))
  margeffects
  assign(paste0("margefx",h1_outcome),margeffects)

  dwplot(me_h1)
  rm(me_h1,h1_outcome,h1_data)

  #### Mixed effect linear model hurdle 2 - how much to spray ####
  h2_outcome <- paste0("lnKg",i,"Ha100")
  h2_data <- filter(data,eval(as.name(h2_outcome)) > 0)
  # h2_data = sample_n(h2_data,1000)
  
  timestamp()
  me_h2_fid <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id), data = h2_data)
  me_h2_perm <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|permit), data = h2_data)
  me_h2_year <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|year), data = h2_data)
  
  me_h2_fidyr <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|year), data = h2_data)
  me_h2_fidperm <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|permit), data = h2_data)
  me_h2_yrperm <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|permit) + (1|year), data = h2_data)
  
  me_h2_all <- lmer(eval(as.name(h2_outcome)) ~  pur_cdfa_org + lnhectares + soil_quality + lnfarmSz + (1|family_id) + (1|permit) + (1|year), data = h2_data)
  
  # AIC(me_h2_fid ,me_h2_perm, me_h2_year, me_h2_fidyr, me_h2_fidperm, me_h2_yrperm, me_h2_all)
  # BIC(me_h2_fid ,me_h2_perm, me_h2_year, me_h2_fidyr, me_h2_fidperm, me_h2_yrperm, me_h2_all)
  anova(me_h2_fid ,me_h2_perm, me_h2_year, me_h2_fidyr, me_h2_fidperm, me_h2_yrperm, me_h2_all)
  timestamp()
  
  assign(paste0("lm_",h1_outcome), me_h2)
  timestamp()
  dwplot(me_h2)
  rm(me_h2,h2_outcome,h2_data)
}






