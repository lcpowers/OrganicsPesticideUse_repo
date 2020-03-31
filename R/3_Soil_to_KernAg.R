
Soil_to_KernAg_fun = function(year){

  ## Read in raw Kern County Agriculture shapefile
  ag_raw = readOGR(paste0("../R_input/spatial/kern_AG_shp/kern",year,"/kern",year,".shp"))
  
  ## Put into California Teale Albers
  ag = spTransform(ag_raw, CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
  
  ## Change into simple features dataframe
  ag_sf = st_as_sf(ag) %>% 
    filter(P_STATUS == "A") # Keep active permits
  
  ## Extract values from soil raster that correspond to each polygon in the ag shapefile
  r.vals = raster::extract(soil_ras, # Storie Index Raster Values
                           ag_sf, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                           fun = mean, # Find the mean value of raster cells that overlap with each polygon
                           small = TRUE, # Include mean values for small polygons
                           weights = TRUE, # Add weights to be able to calculate weighted mean in the next step
                           normalizeWeights = TRUE,
                           na.rm = T, # Ignore NAs when calculating mean values
                           df = TRUE) # Return results as a data.frame
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_sf$soil = r.vals$layer  
  
  ## Convert back into Shapefile
  ag_shp_withSoil = as(ag_sf, "Spatial")
  
  ## Write output shapefile
  writeOGR(obj = ag_shp_withSoil,
           dsn = paste0("../R_output/spatial/KernAg_withSoil/",year,"/"),
           layer = paste0("KernAg_withSoil_",year),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
  
  }