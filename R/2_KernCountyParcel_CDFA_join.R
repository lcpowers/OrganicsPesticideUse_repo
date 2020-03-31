
KernCoParcel_CDFA_join = function(year,kern_parcel_shp){
  
  # Read in CSV of edited APNs for year being evaluated
  cdfa_df = read_csv(paste0("../R_input/CSV/CDFA/final/CDFA_APNs_",year,".csv"))
  
  # Join CSV data to Kern County Parcel data on APN
  join <- merge(kern_parcel_shp, # File x
                cdfa_df, # File y
                by.x = "APN_LABEL", # Field in x to join on
                by.y = "edited_APN", # Field in y to join on
                duplicateGeoms = TRUE) # If there are multiple matches return all of them. 
  
  # Subset to maintain only parcels that had APN match to create a new shapefile of APN parcels. 
  subset_cdfa <- subset(join, cdfa==1)
  
  # Convert the shapefile to a simple features dataframe and remove unnecessary columns. 
  join_sf = st_as_sf(subset_cdfa) %>% 
    dplyr::select(-2,-3,-4,-5,-6,-7,-8,-10)
  
  # Rename columns
  colnames(join_sf) <- c("match_apn",
                         "company",
                         "cdfa_apn",
                         "cdfa",
                         "geometry")
  
  # Convert simple features dataframe back into shapefile with polygons
  final = as(join_sf,"Spatial")
  
  # Write output shapefile into R_output directory
  writeOGR(final, dsn = paste0("../R_output/spatial/CDFA_APN_parcels/",year), 
                              driver="ESRI Shapefile", 
                              layer=paste0("CDFA_Parcels_",year),
                              overwrite = TRUE)

  return(join_sf)
  
}
