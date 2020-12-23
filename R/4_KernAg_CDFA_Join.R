
kernAg_CDFA_joinFun = function(year, buf_width, write_all_shp=F, write_cdfa_shp = F){
  
  ### Load Kern County Agriculture shapefile
  kern_ag_sf = read_sf(paste0("../R_output/spatial/KernAg_withSoil/",year,"/KernAg_withSoil_",year,".shp")) %>% 
    st_transform(., CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
  
  ### Load CDFA parcel shapefile
  cdfa_prcl_sf = read_sf(paste0("../R_output/spatial/CDFA_APN_parcels/",year,"/cdfa_parcels_",year,".shp")) %>% 
    st_transform(., CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) 
  
  
  ######### Joining data ##########
  
  ### Join the Kern County Ag and CDFA parcel spatial data.frames
  join = st_join(kern_ag_sf, st_buffer(cdfa_prcl_sf,-10), largest = T)
  
  ####### Write data before filtering for parcels designated as organic by CDFA APN match #######
  join = join %>% 
    dplyr::select(PERMIT,
                  PMT_SITE,
                  S_STATUS,
                  P_STATUS,
                  PERMITTEE,
                  "COMPANY" = company,
                  COMM,
                  COMM_CODE,
                  "GENUS",
                  "AGROCLASS",
                  "FAMILY",
                  "CDFA"=cdfa,
                  ACRES,
                  "SOIL_withNAs" = soil_NA,
                  "SOIL_noNAs" = soil_noNA,
                  "SOIL_chem" = soil_chem,
                  "SOIL_temp" = soil_temp,
                  "SOIL_hydro" = soil_hydro)
  
  # Separate the COMM column by '-' into COMM_x and COMM_y
  output = join %>%
    separate(col = "COMM",
             into = c("COMM_x","COMM_y"),
             sep = "-",
             remove = FALSE)

  # Convert all COMM columns from factors to character strings
  output$COMM=as.character(output$COMM)
  output$COMM_x=as.character(output$COMM_x)
  output$COMM_y=as.character(output$COMM_y)

  # If some portion of the word 'ORGANIC' is in the COMM_y column, keep only the COMM_x column,
  # otherwise keep the original value in the COMM column
  output$COMM_new <- ifelse(str_detect(output$COMM_y,"OR")|is.na(output$COMM_y),output$COMM_x,output$COMM)
  
  # Remove the COMM_x and COMM_y columns
  output = output %>%
    dplyr::select(-COMM_x,-COMM_y)

  # Replace NAs in the CDFA column with 0s
  output$CDFA = ifelse(is.na(output$CDFA),0,1)
  
  assign(paste0("cdfa_kernag_",year),output,envir = .GlobalEnv)
  
  write_csv(output,
            paste0("../R_output/CSV/CDFA_KernAg_join/CDFA_KernAg_all",year,".csv"))
  
  all_shp <- as(output,"Spatial")
  
  if(write_all_shp){writeOGR(all_shp,
                       paste0("../R_output/spatial/CDFA_KernAg_join/",year,"/buffer",buf_width),
                       paste0("KernAg_CDFA_all_",year,"_B",buf_width),
                       driver = "ESRI Shapefile",
                       overwrite_layer = TRUE)}
  
  ### Select rows with CDFA matches
  cdfa_sf = join %>% 
    filter(CDFA == "1")
  
  ### Write CSV to output directory
  write_csv(cdfa_sf,
            paste0("../R_output/CSV/CDFA_KernAg_join/",year,"/CDFA_KernAg_APNorg_",year,"_buf",buf_width,".csv"))
  
  ### Convert back into shapefile
  cdfa_shp <- as(cdfa_sf,"Spatial")
  
  ### Write shapefile to output directory
 if(write_cdfa_shp)
   {writeOGR(cdfa_shp,
           paste0("../R_output/spatial/CDFA_KernAg_join/",year,"/buffer",buf_width),
           paste0("KernAg_CDFA_apnOrgs_",year,"_B",buf_width),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)}

}


