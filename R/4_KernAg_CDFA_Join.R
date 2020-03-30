
kernAg_CDFA_joinFun = function(year, buf_width, write_all_shp=F, write_cdfa_shp = F){
  
  ### Load Kern County Agriculture shapefile
  kern_ag_shp = readOGR(paste0("../R_input/spatial/kern_AG_withStorInd/",year,"/KernAg_with_StorInd_",year,".shp"))
  
  ### Put into California Teale Albers
  kern_ag_shp = spTransform(kern_ag_shp, CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
  
  ### Turn shapefile attribute table into a spatial dataframe
  kern_ag_sf = st_as_sf(kern_ag_shp)
  
  ### Load CDFA parcel shapefile
  cdfa_prcl_shp = readOGR(paste0("../R_output/spatial/CDFA_Parcels/",year,"/CDFA_Parcels",year,".shp"))
  
  ### Create a buffer around each polygon
  cdfa_prcl_buffer = gBuffer(cdfa_prcl_shp, width = -buf_width, byid = TRUE)
  
  ### Turn CDFA shapefile attribute table into a spatial dataframe
  cdfa_prcl_sf = st_as_sf(cdfa_prcl_buffer)
  
  ######### Joining data ##########
  
  ### Join the Kern County Ag and CDFA parcel spatial dataframes
  join = st_join(kern_ag_sf, cdfa_prcl_sf, left = TRUE, largest = TRUE)
  
  ####### Write data before filtering for parcels desginted as organic by CDFA APN match #######
  join = join %>% 
    dplyr::select(PERMIT,
                  SITEID,
                  PMT_SITE,
                  S_STATUS,
                  PERMITTEE,
                  "COMPANY" = company
                  ,COMM,
                  CDFA,
                  ACRES,
                  "STORIE" = soil,
                  "COMM_GROUP" = SYMBOL)
  
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

  # If the word 'ORGANIC' is in the COMM_y column, keep only the COMM_x column,
  # otherwise keep the original value in the COMM column
  output$COMM_new <- ifelse(output$COMM_y == "ORGANIC"|is.na(output$COMM_y), output$COMM_x,output$COMM)

  # Combine all lettuce categories into one
  output$COMM_new <- ifelse(str_detect(output$COMM_new,"LETTUCE"),"LETTUCE",output$COMM_new)

  # Combine fruit
  output$COMM_GROUP <- ifelse(str_detect(output$COMM_GROUP,"FRUIT"),"FRUIT",as.character(output$COMM_GROUP))
  
  # Remove the COMM_x and COMM_y columns
  output = output %>%
    dplyr::select(-COMM_x,-COMM_y)

  # Replace NAs in the CDFA column with 0s
  output$CDFA = ifelse(is.na(output$CDFA),0,1)
  
  write_csv(output,
            paste0("../R_output/CSV/CDFA_KernAg_join/","CDFA_KernAg_join",year,".csv"))
  
  all_shp <- as(output,"Spatial")
  
  if(write_all_shp)
    {writeOGR(all_shp,
           paste0("../R_output/spatial/KernAg_CDFA_join/",year,"/buffer",buf_width),
           paste0("All_KernAg_CDFA_join",year,"_B",buf_width),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)}
  
  ### Select rows with CDFA matches
  cdfa_sf = join %>% 
    filter(CDFA == "1")
  
  ### Write CSV to output directory
  write_csv(cdfa_sf,
            paste0("../R_output/CSV/CDFA/Commodities/",year,"/CDFA_crops_",year,"_buf",buf_width,".csv"))
  
  ### Convert back into shapefile
  cdfa_shp <- as(cdfa_sf,"Spatial")
  
  ### Write shapefile to output directory
 if(write_cdfa_shp)
   {writeOGR(cdfa_shp,
           paste0("../R_output/spatial/CDFA_commodities/",year,"/buffer",buf_width),
           paste0("CDFA_crops",year,"_B",buf_width),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)}

}



