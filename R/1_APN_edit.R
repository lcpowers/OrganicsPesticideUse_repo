# Function to edit CDFA APNs that are ultimately matched to Kern County Parcel APNs

APN_edit_fun = function(input_df,id_cols,apn_cols,col_names,year){
  
  # Melt data.frame -- go from wide to long table format. Companies/permits with multiple APNs 
  # end up with 
  melt_df = melt(input_df, id.vars = id_cols, measure.vars = apn_cols, na.rm = TRUE)
  
  print(nrow(melt_df))
  
  # Column names
  colnames(melt_df) = col_names

  separate_apns = melt_df %>% 
    dplyr::select(original_apn) %>% 
    separate(col = c("original_apn"), into = c("a","b","c","d","e"))
  
  # Trim any string in column A that is greater than 3 numbers to be exactly 3 numbers long
  separate_apns$a <- strtrim(separate_apns$a, width=3)
  # If a string in column A is less than 3 digits long, pad on the left side with a 0
  separate_apns$a <- str_pad(separate_apns$a, width=3, side="left", pad="0")
  
  # Trim any string in column B >3 digits
  separate_apns$b <- strtrim(separate_apns$b, width=3)
  # Pad column B with a 0 on the right side
  separate_apns$b <- str_pad(separate_apns$b, width=3, side="right", pad="0")
  
  # Trim any digit in column C >2 digits
  separate_apns$c <- strtrim(separate_apns$c, width=2)
  # Pad column C with a 0 on the right side
  separate_apns$c <- str_pad(separate_apns$c, width=2, side="right", pad="0")
  
  # Combine the APN segments from above (A, B, and C) into a single APN with 
  # the segments separated by a '-'
  combined_apns <- as.vector(unite(data = separate_apns[,1:3], col = "edited_APN", sep = "-"))
  
  # Add the edited APNs to the long dataframe so that the original and edited versions are 
  # side-by-side
  output_df <- data.frame(melt_df,combined_apns)
  
  # Create a column of 1s to be used as a binary CDFA/nonCDFA indicator later. 
  # After joins the Kern County Parcel and Kern County Agriculture shapefiles, 
  # this column will be used to identify which observations/rows have CDFA data 
  # associated with them
  output_df$cdfa <- rep(1,nrow(output_df))
  
  # Write output CSVs into both the Rinput and Routput folders for convenience. 
  write_csv(output_df,paste0("../R_output/CSV/CDFA/final_APN/CDFA_APNs_",year,".csv"), append = F)
  write_csv(output_df,paste0("../R_input/CSV/CDFA/final_APN/CDFA_APNs_",year,".csv"), append = F)
  
  # Return a dataframe of results in the Rmarkdown document 
  return(output_df)
  
}
