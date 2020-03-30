

KernAgOrgs_fun = function(year){
  
  input_shp = readOGR(paste0("../R_input/spatial/kern_AG_withStorInd/",year,"/kernAg_with_StorInd_",year,".shp"))
  
  input_sf = st_as_sf(input_shp)
  
  input_sf$COMM = as.character(input_sf$COMM)
  
  organics_fltr = input_sf %>% 
    filter(P_STATUS == "A") %>% 
    filter(str_detect(COMM, "ORGANIC"))
    
  # str_detect(PERMITTEE,"ORGANIC")|str_detect(SITEID,"ORG") & !str_detect(COMM,"SORGHUM")

  organics_fltr$KernAgOrg = rep(1,nrow(organics_fltr))
  
  organics_fltr_shp = as(organics_fltr,"Spatial")
  
  organics_fltr_shp <- spTransform(organics_fltr_shp, 
                                   CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) 
  
  dir.create(paste0("../R_output/spatial/KernAg_organics/",year))
  
  writeOGR(organics_fltr_shp, 
           dsn = paste0("../R_output/spatial/KernAg_organics/",year), 
           driver="ESRI Shapefile", 
           layer=paste0("KernAg_Orgs",year),
           overwrite_layer = TRUE)
  
}
