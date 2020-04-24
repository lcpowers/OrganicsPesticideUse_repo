#This script performs the xtsum analysis (similar to XTSUM capabilities in STATA) on the dataframe. 
  #The XTSUM function is created to perform the analysis, and then applied to each of the variables to be potentially used in the regressions.
  #The results are then combined across variables and saved.

#Load Packages
library(rlang)
library(dplyr)
library(gdata)
library(tidyverse)

#Create xtsum function for one group at a time
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
data<-df %>% 
  dplyr::mutate(KgAerialAIperHE=KgAerialAI/hectares,
                GroundAIperHE=GroundAI/hectares, 
                OtherAIperHE=OtherAI/hectares,
                KgAiWaterperHE=KgAiWater/hectares,
                KgAiWildlifeperHE=KgAiWildlife/hectares,
                KgAiMammalperHE=KgAiMammal/hectares,
                KgAiBirdsperHE=KgAiBirds/hectares,
                KgAiFishperHE=KgAiFish/hectares,
                KgAiBeesperHE=KgAiBees/hectares,
                KgAiEndgSpperHE=KgAiEndgSp/hectares,
                KgAiAqSpperHE=KgAiAqSp/hectares,
                KgAiGrndWatperHE=KgAiGrndWat/hectares,
                KgAiDriftperHE=KgAiDrift/hectares,
                KgAiReptAmphperHE=KgAiReptAmph/hectares,
                KgAInsectOnlyperHE=KgAInsectOnly/hectares,
                KgAHerbOnlyperHE=KgAHerbOnly/hectares,
                KgAFungOnlyperHE=KgAFungOnly/hectares,
                AiInsFngperHE=AiInsFng/hectares) %>% 
  dplyr::select(permitsite, comm_code, year,soil_quality, all_org, hectares, 
                KgAIperHE,KgPRDperHE, KgAerialAIperHE,GroundAIperHE,OtherAIperHE,
                KgAiWaterperHE, KgAiWildlifeperHE, KgAiMammalperHE, KgAiBirdsperHE,
                KgAiFishperHE, KgAiBeesperHE, KgAiEndgSpperHE, KgAiAqSpperHE, KgAiGrndWatperHE,
                KgAiDriftperHE, KgAiReptAmphperHE, KgAInsectOnlyperHE, KgAHerbOnlyperHE, 
                KgAFungOnlyperHE,AiInsFngperHE) %>% 
  dplyr::rename(soilquality=soil_quality, allorg=all_org)



#Perform xtsum function for each variable being considered in the regression, for both permit number and commodity number
  #Have to perform each variable separately based on how xtsumR function created (global variable)
  
  #allorg
    allorg_permit<-XTSUM(data, varname=allorg, unit=permitsite)
    allorg_comm<-XTSUM(data, varname=allorg, unit=comm_code)
  #hectares
    hectares_permit<-XTSUM(data, varname=hectares, unit=permitsite)
    hectares_comm<-XTSUM(data, varname=hectares, unit=comm_code)
  #soilquality
    soilquality_permit<-XTSUM(data, varname=soilquality, unit=permitsite)
    soilquality_comm<-XTSUM(data, varname=soilquality, unit=comm_code)
  #KgAIperHE
    KgAIperHE_permit<-XTSUM(data, varname=KgAIperHE, unit=permitsite)
    KgAIperHE_comm<-XTSUM(data, varname=KgAIperHE, unit=comm_code)
  #KgPRDperHE
    KgPRDperHE_permit<-XTSUM(data, varname=KgPRDperHE, unit=permitsite)
    KgPRDperHE_comm<-XTSUM(data, varname=KgPRDperHE, unit=comm_code)
  #KgAerialAIperHE
    KgAerialAIperHE_permit<-XTSUM(data, varname=KgAerialAIperHE, unit=permitsite)
    KgAerialAIperHE_comm<-XTSUM(data, varname=KgAerialAIperHE, unit=comm_code)
  #GroundAIperHE
    GroundAIperHE_permit<-XTSUM(data, varname=GroundAIperHE, unit=permitsite)
    GroundAIperHE_comm<-XTSUM(data, varname=GroundAIperHE, unit=comm_code)
  #OtherAIperHE
    OtherAIperHE_permit<-XTSUM(data, varname=OtherAIperHE, unit=permitsite)
    OtherAIperHE_comm<-XTSUM(data, varname=OtherAIperHE, unit=comm_code)
  #KgAiWaterperHE
    KgAiWaterperHE_permit<-XTSUM(data, varname=KgAiWaterperHE, unit=permitsite)
    KgAiWaterperHE_comm<-XTSUM(data, varname=KgAiWaterperHE, unit=comm_code)
  #KgAiWildlifeperHE
    KgAiWildlifeperHE_permit<-XTSUM(data, varname=KgAiWildlifeperHE, unit=permitsite)
    KgAiWildlifeperHE_comm<-XTSUM(data, varname=KgAiWildlifeperHE, unit=comm_code)
  #KgAiMammalperHE
    KgAiMammalperHE_permit<-XTSUM(data, varname=KgAiMammalperHE, unit=permitsite)
    KgAiMammalperHE_comm<-XTSUM(data, varname=KgAiMammalperHE, unit=comm_code)
  #KgAiBirdsperHE
    KgAiBirdsperHE_permit<-XTSUM(data, varname=KgAiBirdsperHE, unit=permitsite)
    KgAiBirdsperHE_comm<-XTSUM(data, varname=KgAiBirdsperHE, unit=comm_code)
  #KgAiFishperHE
    KgAiFishperHE_permit<-XTSUM(data, varname=KgAiFishperHE, unit=permitsite)
    KgAiFishperHE_comm<-XTSUM(data, varname=KgAiFishperHE, unit=comm_code)
  #KgAiBeesperHE
    KgAiBeesperHE_permit<-XTSUM(data, varname=KgAiBeesperHE, unit=permitsite)
    KgAiBeesperHE_comm<-XTSUM(data, varname=KgAiBeesperHE, unit=comm_code)
  #KgAiEndgSpperHE
    KgAiEndgSpperHE_permit<-XTSUM(data, varname=KgAiEndgSpperHE, unit=permitsite)
    KgAiEndgSpperHE_comm<-XTSUM(data, varname=KgAiEndgSpperHE, unit=comm_code)
  #KgAiAqSpperHE
    KgAiAqSpperHE_permit<-XTSUM(data, varname=KgAiAqSpperHE, unit=permitsite)
    KgAiAqSpperHE_comm<-XTSUM(data, varname=KgAiAqSpperHE, unit=comm_code)
  #KgAiGrndWatperHE
    KgAiGrndWatperHE_permit<-XTSUM(data, varname=KgAiGrndWatperHE, unit=permitsite)
    KgAiGrndWatperHE_comm<-XTSUM(data, varname=KgAiGrndWatperHE, unit=comm_code)
  #KgAiDriftperHE
    KgAiDriftperHE_permit<-XTSUM(data, varname=KgAiDriftperHE, unit=permitsite)
    KgAiDriftperHE_comm<-XTSUM(data, varname=KgAiDriftperHE, unit=comm_code)
  #KgAiReptAmphperHE
    KgAiReptAmphperHE_permit<-XTSUM(data, varname=KgAiReptAmphperHE, unit=permitsite)
    KgAiReptAmphperHE_comm<-XTSUM(data, varname=KgAiReptAmphperHE, unit=comm_code)
    #KgAInsectOnlyperHE
    KgAInsectOnlyperHE_permit<-XTSUM(data, varname=KgAInsectOnlyperHE, unit=permitsite)
    KgAInsectOnlyperHE_comm<-XTSUM(data, varname=KgAInsectOnlyperHE, unit=comm_code)
    #KgAHerbOnlyperHE
    KgAHerbOnlyperHE_permit<-XTSUM(data, varname=KgAHerbOnlyperHE, unit=permitsite)
    KgAHerbOnlyperHE_comm<-XTSUM(data, varname=KgAHerbOnlyperHE, unit=comm_code)
    #KgAFungOnlyperHE
    KgAFungOnlyperHE_permit<-XTSUM(data, varname=KgAFungOnlyperHE, unit=permitsite)
    KgAFungOnlyperHE_comm<-XTSUM(data, varname=KgAFungOnlyperHE, unit=comm_code)
    #AiInsFngperHE
    AiInsFngperHE_permit<-XTSUM(data, varname=AiInsFngperHE, unit=permitsite)
    AiInsFngperHE_comm<-XTSUM(data, varname=AiInsFngperHE, unit=comm_code)
      
############################################################
#Both permitsite and comm_code at same time
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
        dplyr::group_by(permitsite, comm_code) %>% 
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
        dplyr::group_by(permitsite, comm_code) %>% 
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
    #allorg
    allorg_permitcomm<-XTSUM_permitcomm(data, varname=allorg)
    #hectares
    hectares_permitcomm<-XTSUM_permitcomm(data, varname=hectares)
    #soilquality
    soilquality_permitcomm<-XTSUM_permitcomm(data, varname=soilquality)
    #KgAIperHE
    KgAIperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAIperHE)
    #KgPRDperHE
    KgPRDperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgPRDperHE)
    #KgAerialAIperHE
    KgAerialAIperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAerialAIperHE)
    #GroundAIperHE
    GroundAIperHE_permitcomm<-XTSUM_permitcomm(data, varname=GroundAIperHE)
    #OtherAIperHE
    OtherAIperHE_permitcomm<-XTSUM_permitcomm(data, varname=OtherAIperHE)
    #KgAiWaterperHE
    KgAiWaterperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiWaterperHE)
    #KgAiWildlifeperHE
    KgAiWildlifeperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiWildlifeperHE)
    #KgAiMammalperHE
    KgAiMammalperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiMammalperHE)
    #KgAiBirdsperHE
    KgAiBirdsperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiBirdsperHE)
    #KgAiFishperHE
    KgAiFishperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiFishperHE)
    #KgAiBeesperHE
    KgAiBeesperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiBeesperHE)
    #KgAiEndgSpperHE
    KgAiEndgSpperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiEndgSpperHE)
    #KgAiAqSpperHE
    KgAiAqSpperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiAqSpperHE)
    #KgAiGrndWatperHE
    KgAiGrndWatperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiGrndWatperHE)
    #KgAiDriftperHE
    KgAiDriftperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiDriftperHE)
    #KgAiReptAmphperHE
    KgAiReptAmphperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAiReptAmphperHE)
    #KgAInsectOnlyperHE
    KgAInsectOnlyperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAInsectOnlyperHE)
    #KgAHerbOnlyperHE
    KgAHerbOnlyperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAHerbOnlyperHE)
    #KgAFungOnlyperHE
    KgAFungOnlyperHE_permitcomm<-XTSUM_permitcomm(data, varname=KgAFungOnlyperHE)
    #AiInsFngperHE
    AiInsFngperHE_permitcomm<-XTSUM_permitcomm(data, varname=AiInsFngperHE)
    
#############################################################################
    #Both permitsite, comm_code, and year at same time
    #Create xtsum function for permit and commodity and year groups together
    XTSUM_permitcommyear<- function(data, varname) {
      varname <- rlang::enquo(varname)
      ores <- data %>% 
        dplyr::summarise(ovr.mean=mean(!! varname, na.rm=TRUE), 
                         ovr.sd=sd(!! varname, na.rm=TRUE), 
                         ovr.min = min(!! varname, na.rm=TRUE), 
                         ovr.max=max(!! varname, na.rm=TRUE), 
                         ovr.N=sum(as.numeric((!is.na(!! varname)))))
      bmeans <- data %>% 
        dplyr::group_by(permitsite, comm_code, year) %>% 
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
        dplyr::group_by(permitsite, comm_code, year) %>% 
        dplyr::mutate(W.x = scale(!! varname, scale=FALSE))
      wres <- wdat %>% 
        dplyr::ungroup() %>% 
        dplyr::summarise(within.sd=sd(W.x, na.rm=TRUE), 
                         within.min=min(W.x, na.rm=TRUE), 
                         within.max=max(W.x, na.rm=TRUE))
      values<-cbind(ores, bres, wres)
      return(values)
    }
    
    #Perform xtsum_permitcommyear for each variable  
    #allorg
    allorg_permitcommyear<-XTSUM_permitcommyear(data, varname=allorg)
    #hectares
    hectares_permitcommyear<-XTSUM_permitcommyear(data, varname=hectares)
    #soilquality
    soilquality_permitcommyear<-XTSUM_permitcommyear(data, varname=soilquality)
    #KgAIperHE
    KgAIperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAIperHE)
    #KgPRDperHE
    KgPRDperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgPRDperHE)
    #KgAerialAIperHE
    KgAerialAIperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAerialAIperHE)
    #GroundAIperHE
    GroundAIperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=GroundAIperHE)
    #OtherAIperHE
    OtherAIperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=OtherAIperHE)
    #KgAiWaterperHE
    KgAiWaterperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiWaterperHE)
    #KgAiWildlifeperHE
    KgAiWildlifeperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiWildlifeperHE)
    #KgAiMammalperHE
    KgAiMammalperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiMammalperHE)
    #KgAiBirdsperHE
    KgAiBirdsperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiBirdsperHE)
    #KgAiFishperHE
    KgAiFishperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiFishperHE)
    #KgAiBeesperHE
    KgAiBeesperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiBeesperHE)
    #KgAiEndgSpperHE
    KgAiEndgSpperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiEndgSpperHE)
    #KgAiAqSpperHE
    KgAiAqSpperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiAqSpperHE)
    #KgAiGrndWatperHE
    KgAiGrndWatperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiGrndWatperHE)
    #KgAiDriftperHE
    KgAiDriftperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiDriftperHE)
    #KgAiReptAmphperHE
    KgAiReptAmphperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAiReptAmphperHE)
    #KgAInsectOnlyperHE
    KgAInsectOnlyperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAInsectOnlyperHE)
    #KgAHerbOnlyperHE
    KgAHerbOnlyperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAHerbOnlyperHE)
    #KgAFungOnlyperHE
    KgAFungOnlyperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=KgAFungOnlyperHE)
    #AiInsFngperHE
    AiInsFngperHE_permitcommyear<-XTSUM_permitcommyear(data, varname=AiInsFngperHE)
    
    
    
#############################################################################
    #Cbind all the results together, using the variable name as identifier
    xtsum_df<-gdata::combine(allorg_permit, allorg_comm,allorg_permitcomm,allorg_permitcommyear,
                             hectares_permit, hectares_comm,hectares_permitcomm,hectares_permitcommyear,
                             soilquality_permit,soilquality_comm,soilquality_permitcomm,soilquality_permitcommyear,
                             KgAIperHE_permit,KgAIperHE_comm,KgAIperHE_permitcomm,KgAIperHE_permitcommyear,
                             KgPRDperHE_permit,KgPRDperHE_comm,KgPRDperHE_permitcomm,KgPRDperHE_permitcommyear,
                             KgAerialAIperHE_permit,KgAerialAIperHE_comm,KgAerialAIperHE_permitcomm,KgAerialAIperHE_permitcommyear,
                             GroundAIperHE_permit,GroundAIperHE_comm,GroundAIperHE_permitcomm,GroundAIperHE_permitcommyear,
                             OtherAIperHE_permit,OtherAIperHE_comm,OtherAIperHE_permitcomm,OtherAIperHE_permitcommyear,
                             KgAiWaterperHE_permit,KgAiWaterperHE_comm,KgAiWaterperHE_permitcomm,KgAiWaterperHE_permitcommyear,
                             KgAiWildlifeperHE_permit,KgAiWildlifeperHE_comm,KgAiWildlifeperHE_permitcomm,KgAiWildlifeperHE_permitcommyear,
                             KgAiMammalperHE_permit,KgAiMammalperHE_comm,KgAiMammalperHE_permitcomm,KgAiMammalperHE_permitcommyear,
                             KgAiBirdsperHE_permit,KgAiBirdsperHE_comm,KgAiBirdsperHE_permitcomm,KgAiBirdsperHE_permitcommyear,
                             KgAiFishperHE_permit, KgAiFishperHE_comm,KgAiFishperHE_permitcomm,KgAiFishperHE_permitcommyear,
                             KgAiBeesperHE_permit, KgAiBeesperHE_comm,KgAiBeesperHE_permitcomm,KgAiBeesperHE_permitcommyear,
                             KgAiEndgSpperHE_permit, KgAiEndgSpperHE_comm,KgAiEndgSpperHE_permitcomm,KgAiEndgSpperHE_permitcommyear,
                             KgAiAqSpperHE_permit, KgAiAqSpperHE_comm,KgAiAqSpperHE_permitcomm,KgAiAqSpperHE_permitcommyear,
                             KgAiGrndWatperHE_permit,KgAiGrndWatperHE_comm,KgAiGrndWatperHE_permitcomm,KgAiGrndWatperHE_permitcommyear,
                             KgAiDriftperHE_permit, KgAiDriftperHE_comm,KgAiDriftperHE_permitcomm,KgAiDriftperHE_permitcommyear,
                             KgAiReptAmphperHE_permit, KgAiReptAmphperHE_comm,KgAiReptAmphperHE_permitcomm,KgAiReptAmphperHE_permitcommyear,
                             KgAInsectOnlyperHE_permit, KgAInsectOnlyperHE_comm,KgAInsectOnlyperHE_permitcomm,KgAInsectOnlyperHE_permitcommyear,
                             KgAHerbOnlyperHE_permit, KgAHerbOnlyperHE_comm, KgAHerbOnlyperHE_permitcomm, KgAHerbOnlyperHE_permitcommyear, 
                             KgAFungOnlyperHE_permit,KgAFungOnlyperHE_comm,KgAFungOnlyperHE_permitcomm,KgAFungOnlyperHE_permitcommyear,
                             AiInsFngperHE_permit,AiInsFngperHE_comm,AiInsFngperHE_permitcomm,AiInsFngperHE_permitcommyear) %>% 
      dplyr::select(source, everything()) %>% 
      separate(source, c("Variable", "Grouping")) %>% 
      dplyr::mutate(Variation=(within.sd/ovr.sd)*100,
                    Var_Check=ifelse(Variation>=10, "Yes", "No"))
  
    #Write csv
    dir.create("R_output/CSV/XTSUM/")
    write_csv(xtsum_df, "R_output/CSV/XTSUM/xtsum_df.csv")
    