#Landscape Metrics for Organic vs Conventional Farms
#Currently not using any of these metrics, but available if necessary
#Read in packages
library(tidyverse)
library(sf)
library(haven)
library(fasterize)
library(raster)
library(landscapemetrics)
library(purrr)
library(doParallel)
options(scipen=999)

#Read in organics shapefile
organics_sf<-sf::read_sf("R_output/spatial/final_organics_data/all_years/all_years_organics.shp") %>% 
  dplyr::rename(permitsite=permtst)

#Read in pesticide data
pest_data<- haven::read_dta("R_input/dta/organicAnalysisPUR.dta")%>% zap_formats() %>% zap_label()

#Combine organics and pesticide data
organics_pest<-left_join(organics_sf, pest_data, by=c("permitsite", "year"))
#Write shapefile of combined data
organics_pest_sf<-organics_pest %>% 
  dplyr::select(permitsite, year, commodity, agroclass, soil_quality, pur_cdfa_org, all_but_PUR_org, hectares, KgPestAI, KgPestPrd)
# dir.create("R_output/spatial/organics_pesticide_data/")
organics_pestsf <- st_collection_extract(organics_pest_sf, "POLYGON")
colnames(organics_pestsf)<-c('permitsite', 'year', 'commodity', 'agroclass', 'soil_quality', 'pur_cdfa_org', 'all_but_PUR_org', 
                              'hectares', 'KgPestAI', 'KgPestPrd', "geometry")
st_write(organics_pestsf, "R_output/spatial/organics_pesticide_data/organics_pestallyrs.shp", delete_layer = TRUE)
#Not currently working

#Create raster that will be used to rasterize all year shapefiles
  #Initialize empty rasters
  ras<-raster()
  #Set the raster extent
  extent(ras) = extent(organics_pest)
  #Set raster resolution (meters)
  res(ras) = 50
  #Define the CRS
  crs(ras) = crs(organics_pest)
  
#Perform landscape metrics for each year
result<-foreach(i=2013:2019, .combine=rbind)%do%{
  #Subset organics to year
  organics_year<-organics_pest %>% dplyr::filter(year==i) %>% dplyr::select(pur_cdfa_org)
  organics_ras<-fasterize(organics_year, ras, field="pur_cdfa_org")
  
  #Landscape Metrics of Organic-Conventional Raster
  ta<-lsm_l_ta(organics_ras, directions = 8) #Total area of landscape in hectares
  pland<-lsm_c_pland(organics_ras, directions = 8)#Percentage of landscape %
  ed<-lsm_l_ed(organics_ras, directions = 8, count_boundary = FALSE)#Landscape Edge Density m/ha (Organic-Conventional)
  mna<-lsm_c_area_mn(organics_ras, directions=8)#Mean patch area in hectares
  ai<-lsm_c_ai(organics_ras)#Aggregation index %, out of 100% (max aggregated)
  coh<-lsm_c_cohesion(organics_ras, directions=8)#Patch Cohesion Index %-aggregation metric, % 0 to 100-where 100 most aggregated
  
  df<-list(ta, ed, pland, mna, ai, coh) %>% 
   purrr::reduce(full_join, by=c("layer", "level", "class", "id", "metric", "value")) %>% 
    dplyr::select(-c(layer, id)) %>% dplyr::mutate(year=i) %>% dplyr::select(year, everything())
  return(df)
  
}

#Final dataframes for class and landscape metrics
landscape_df<-result %>% dplyr::filter(level=="landscape") %>% spread(metric, value) %>% dplyr::select(-c(class, level))
class_df<-result %>% dplyr::filter(level=="class") %>%   spread(metric, value)%>% dplyr::select(-level)

#Save dataframes
dir.create("R_output/CSV/LandscapeMetrics/")
write_csv(landscape_df, "R_output/CSV/LandscapeMetrics/landscape_df.csv")
write_csv(class_df, "R_output/CSV/LandscapeMetrics/class_df.csv")

#Read in csvs if double check
landscape_df<-read_csv("R_output/CSV/LandscapeMetrics/landscape_df.csv")
class_df<-read_csv("R_output/CSV/LandscapeMetrics/class_df.csv")