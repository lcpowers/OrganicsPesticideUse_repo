
KernCoParcel_CDFA_join = function(year,kern_parcel_shp){
  
  timestamp()
  
  # Read in CSV of edited APNs for year being evaluated
  cdfa_df = read_csv(paste0("../R_input/CSV/CDFA/final/CDFA_APNs_",year,".csv"))
  
  # Join CSV data to Kern County Parcel data on APN
  join <- merge(kern_parcel_shp, # File x
                cdfa_df, # File y
                by.x = "APN_LABEL", # Field in x to join on
                by.y = "edited_APN", # Field in y to join on
                duplicateGeoms = TRUE) # If there are multiple matches return all of them. 
  
  # Convert the shapefile to a simple features dataframe and remove unnecessary columns. 
  join_sf = st_as_sf(join) %>% 
    filter(cdfa == 1) %>% 
    dplyr::select(-c("FID","APN","APN9","SHAPE_SQFT","SHAPE_ACRE","Shape__Are","Shape__Len","APN_index"))
  
  # Rename columns
  colnames(join_sf) <- c("joind_apn",
                         "company",
                         "cdfa_apn",
                         "cdfa",
                         "geometry")
  
  assign(paste0("cdfa_parcels_",i),join_sf,envir=.GlobalEnv)
  
  # Convert simple features dataframe back into shapefile with polygons
  final = as(join_sf,"Spatial")
  
  # Write output shapefile into R_output directory
  writeOGR(final, dsn = paste0("../R_output/spatial/CDFA_APN_parcels/",year), 
                              driver="ESRI Shapefile", 
                              layer=paste0("cdfa_parcels_",year),
                              overwrite = TRUE)

  
}
