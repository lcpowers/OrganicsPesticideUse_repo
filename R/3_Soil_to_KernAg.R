
Soil_to_KernAg_fun = function(year){

  timestamp()
  
  ### Read in raw Kern County Agriculture shapefile ###
  ag_sf = read_sf(paste0("../R_input/spatial/kern_AG_shp/kern",year,"/kern",year,".shp")) %>% 
    st_transform(.,CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) %>% 
    # filter(P_STATUS == "A" & !str_detect(COMM,"UNCULTIVATED")) %>%  # Keep active permits
    mutate(AG_TRS = paste0(TOWNSHIP,RANGE,str_pad(SECTION,2,pad="0",side=c("left")))) %>% 
    dplyr::select(PERMIT,PERMITTEE,PMT_SITE,COMM,S_STATUS,P_STATUS,ACRES,AG_TRS) 
  
  ###### Deal with the rows where all data is exactly the same, except geometries are different. 
  ag_sf = ag_sf %>% 
    group_by(PERMIT,PERMITTEE,PMT_SITE,COMM,S_STATUS,P_STATUS,ACRES,AG_TRS) %>% 
    summarise()
  
  ag_sf$COMM <- as.character(ag_sf$COMM)
  # ag_sf$COMM_CODE<- as.character(ag_sf$COMM_CODE)
  
  ag_merge <- left_join(ag_sf,agro_class, 
                    by = c("COMM"))
  
  # ag_merge_check <- ag_merge %>% 
  #  as.data.frame(.) %>% 
  #  filter(is.na(FAMILY))
 
  ag_slct <- ag_merge %>% 
    dplyr::select(-c(COMM_EDIT,COMM_CODEOLD))
  
  ## Extract values from soil raster that correspond to each polygon in the ag shapefile
  r.vals = raster::extract(soil_ras, # Storie Index Raster Values
                           ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                           fun = mean, # Find the mean value of raster cells that overlap with each polygon
                           small = TRUE, # Include mean values for small polygons
                           weights = TRUE, # Add weights for weighted means
                           normalizeWeights = TRUE,
                           na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                           df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals$layer[r.vals$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil = r.vals$layer  
  
  ## Convert back into Shapefile
  ag_shp_withSoil = as(ag_slct, "Spatial")
  
  #Create year subfolder
  dir.create(paste0("../R_output/spatial/KernAg_withSoil/", year))
  
  ## Write output shapefile
  writeOGR(obj = ag_shp_withSoil,
           dsn = paste0("../R_output/spatial/KernAg_withSoil/",year),
           layer = paste0("KernAg_withSoil_",year),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
  
  }