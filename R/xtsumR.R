#This script performs the xtsum analysis (similar to XTSUM capabilities in STATA) on the dataframe. 
  #The XTSUM function is created to perform the analysis, and then applied to each of the variables to be potentially used in the regressions.
  #The results are then combined across variables and saved.

#Load Packages
library(rlang)
library(dplyr)
library(gdata)
library(tidyverse)

#Create xtsum function
XTSUM <- function(data, varname, unit) {
  varname <- rlang::enquo(varname)
  loc.unit <- rlang::enquo(unit)
  ores <- data %>% 
    dplyr::summarise(ovr.mean=mean(!! varname, na.rm=TRUE), 
              ovr.sd=sd(!! varname, na.rm=TRUE), 
              ovr.min = min(!! varname, na.rm=TRUE), 
              ovr.max=max(!! varname, na.rm=TRUE), 
              ovr.N=sum(as.numeric((!is.na(!! varname)))))
  bmeans <- data %>% 
    dplyr::group_by(!! loc.unit) %>% 
    dplyr::summarise(meanx=mean(!! varname, na.rm=T), 
              t.count=sum(as.numeric(!is.na(!! varname))))
  bres <- bmeans %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(between.sd = sd(meanx, na.rm=TRUE), 
              between.min = min(meanx, na.rm=TRUE), 
              between.max=max(meanx, na.rm=TRUE), 
              Units=sum(as.numeric(!is.na(t.count))), 
              t.bar=mean(t.count, na.rm=TRUE))
  wdat <- data %>% 
    dplyr::group_by(!! loc.unit) %>% 
    dplyr::mutate(W.x = scale(!! varname, scale=FALSE))
  wres <- wdat %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(within.sd=sd(W.x, na.rm=TRUE), 
              within.min=min(W.x, na.rm=TRUE), 
              within.max=max(W.x, na.rm=TRUE))
  values<-cbind(ores, bres, wres)
  return(values)
}

#Read in the dataset
df<-readr::read_csv("R_output/CSV/compiled_organics1317.csv")
data<-df %>% dplyr::select(permitsite, comm_edit, KgPerHE, soil_quality, cdfa_org, hectares)


#Perform xtsum function for each variable being considered in the regression, for both permit number, commodity number, and both
  #Have to perform each variable separately based on how xtsumR function created (global variable)
  #KgPerHE
      KgPerHE_permit<-XTSUM(data, varname=KgPerHE, unit=permitsite)
      KgPerHE_comm<-XTSUM(data, varname=KgPerHE, unit=comm_edit)
  #soil_quality
      soil_quality_permit<-XTSUM(data, varname=soil_quality, unit=permitsite)
      soil_quality_comm<-XTSUM(data, varname=soil_quality, unit=comm_edit)
  #cdfa_org
      cdfa_org_permit<-XTSUM(data, varname=cdfa_org, unit=permitsite)
      cdfa_org_comm<-XTSUM(data, varname=cdfa_org, unit=comm_edit)
  #hectares
      hectares_permit<-XTSUM(data, varname=hectares, unit=permitsite)
      hectares_comm<-XTSUM(data, varname=hectares, unit=comm_edit)
      
############################################################
#Both permitsite and comm_edit at same time
    #Create xtsum function for permit and commodity groups together
    XTSUM_permitcomm<- function(data, varname) {
      varname <- rlang::enquo(varname)
      ores <- data %>% 
        dplyr::summarise(ovr.mean=mean(!! varname, na.rm=TRUE), 
                         ovr.sd=sd(!! varname, na.rm=TRUE), 
                         ovr.min = min(!! varname, na.rm=TRUE), 
                         ovr.max=max(!! varname, na.rm=TRUE), 
                         ovr.N=sum(as.numeric((!is.na(!! varname)))))
      bmeans <- data %>% 
        dplyr::group_by(permitsite, comm_edit) %>% 
        dplyr::summarise(meanx=mean(!! varname, na.rm=T), 
                         t.count=sum(as.numeric(!is.na(!! varname))))
      bres <- bmeans %>% 
        dplyr::ungroup() %>% 
        dplyr::summarise(between.sd = sd(meanx, na.rm=TRUE), 
                         between.min = min(meanx, na.rm=TRUE), 
                         between.max=max(meanx, na.rm=TRUE), 
                         Units=sum(as.numeric(!is.na(t.count))), 
                         t.bar=mean(t.count, na.rm=TRUE))
      wdat <- data %>% 
        dplyr::group_by(permitsite, comm_edit) %>% 
        dplyr::mutate(W.x = scale(!! varname, scale=FALSE))
      wres <- wdat %>% 
        dplyr::ungroup() %>% 
        dplyr::summarise(within.sd=sd(W.x, na.rm=TRUE), 
                         within.min=min(W.x, na.rm=TRUE), 
                         within.max=max(W.x, na.rm=TRUE))
      values<-cbind(ores, bres, wres)
      return(values)
    }
    
  #Perform xtsum_permitcomm for each variable  
    #KgPerHE
    KgPerHE_permitcomm<-XTSUM_permitcomm(data, varname=KgPerHE)
    #soil_quality
    soil_quality_permitcomm<-XTSUM_permitcomm(data, varname=soil_quality)
    #cdfa_org
    cdfa_org_permitcomm<-XTSUM_permitcomm(data, varname=cdfa_org)
    #hectares
    hectares_permitcomm<-XTSUM_permitcomm(data, varname=hectares)
    
#############################################################################
    #Cbind all the results together, using the variable name as identifier
    xtsum_df<-gdata::combine(KgPerHE_permit, KgPerHE_comm, KgPerHE_permitcomm,
                             soil_quality_permit, soil_quality_comm, soil_quality_permitcomm,
                             cdfa_org_permit, cdfa_org_comm, cdfa_org_permitcomm,
                             hectares_permit, hectares_comm, hectares_permitcomm) %>% 
      select(source, everything())
  
    #Write csv
    dir.create("R_output/CSV/XTSUM/")
    write_csv(xtsum_df, "R_output/CSV/XTSUM/xtsum_df.csv")
    