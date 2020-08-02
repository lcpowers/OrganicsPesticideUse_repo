library(tidyverse)
library(fuzzyjoin)

agrian_db = read_csv("../R_input/CSV/pesticide_IDs/Agrian_OrganicsOnly.csv")
unknownpests = read_csv("../R_input/CSV/pesticide_IDs/UnknownOrganicProducts.csv")

top_unknownpests = unknownpests %>% 
  filter(!is.na(ORGANIC))

organic_unknownpests = top_unknownpests %>% 
  filter(ORGANIC == "YES")

# fuzzymatches = fuzzy_inner_join(agrian_db$productname, 
#                                unknownpests$productname) 
# Consistently getting this error message when using the fuzzyjoin package...Error in UseMethod("as.tbl") : no applicable method for 'as.tbl' applied to an object of class "factor"

# Trying the agrep appraoch: <https://astrostatistics.psu.edu/su07/R/html/base/html/agrep.html>


for(i in 1:nrow(organic_unknownpests)){
  
  print(agrep(organic_unknownpests$productname[i], agrian_db$productname, ignore.case = T, value = F))

}

## All six were matched to at least one entry in the the Agrian DB

name_matches = as.data.frame(matrix(ncol=2, nrow=nrow(top_unknownpests)))
colnames(name_matches) = c("unknownpest_name","agrian_rowindex")

for(i in 1:nrow(top_unknownpests)){
  #tmp <- data.frame(agrian_rowindex = agrep(top_unknownpests$productname[i], agrian_db$productname, ignore.case = T, value = F))
  print(i)
  name_matches[i,1] = top_unknownpests$productname[i]
  tmp = c(agrep(top_unknownpests$productname[i], agrian_db$productname, max = 2, ignore.case = T, value = F))

  name_matches[i,2] =  as.character(tmp[1])
  
  }

number_matches = as.data.frame(matrix(ncol=2, nrow=nrow(top_unknownpests)))
colnames(number_matches) = c("unknownpest_regno","agrian_rowindex")

for(i in 1:nrow(top_unknownpests)){
  #tmp <- data.frame(agrian_rowindex = agrep(top_unknownpests$productname[i], agrian_db$productname, ignore.case = T, value = F))
  print(i)
  number_matches[i,1] = top_unknownpests$regno[i]
  tmp = c(agrep(top_unknownpests$epa[i], agrian_db$regno, ignore.case = T, value = F))
  
  number_matches[i,2] =  as.character(tmp[1])
  
}







