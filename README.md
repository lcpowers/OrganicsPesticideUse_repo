Will update README ASAP

organics_repo file structure

****** All file pathways are relative to the inside of the Rmarkdown folder *******

#### R
  This folder contains 4 .R documents/scripts that contain functions used on RMD files. The numerical prefix corresponds to the numerical prefix of the RMD document in which it is used

#### R_input
  This folder contains input files. It is too big to load onto and store on GitHub, but is in a zip folder on Google Drive
  
#### R_output
  Folder that output files are written to. These also serve as input files as they are created. The folder should intiate automatically as you run through the RMD
  
#### Rmarkdown
  This contains all the Rmarkdown files. The numerical prefix indicates the order in which to use these (and also the .R file that is sourced, if any)
  These are being fintuned still beyond #6
  When they have an 'a' or 'b' suffix:
    - 'a' = cdfa organics
    - 'b' = cdfa and kern ag organics (those identified using indicators from cdfa data AND kern ag data)
