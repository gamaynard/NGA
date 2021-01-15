## ---------------------------
## Script name: DealerStandardization.R
##
## Purpose of script: The dealer data that is available through the SIMM to sector
##    managers and individual fishermen is not directly compatible with EM data 
##    or electronic vessel trip report data. This script standardizes the data
##    formats. 
##
## Author: George A. Maynard
##
## Date Created: 2021-01-15
##
## Copyright (c) George A. Maynard, 2021
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
## Notes:
##   
## ---------------------------
## Set Working Directory
##
## ---------------------------
## Set options as necessary
options(scipen = 6, digits = 4) ## Disable Scientific Notation
##
## ---------------------------
## Load necessary packages
library(lubridate)
##
## ---------------------------
## Load any custom functions
##
## ---------------------------
d=choose.dir()
fileList=dir(d)
Dealer=data.frame()
## Most dealer data are delivered in .csv files, but the Sustainable Harvest Sector
## generally delivers theirs as .xlsx or .xls. You may need to edit this section
## depending on the breakdown of file types. All should have the same data 
## structure regardless of the file format. 
for(i in 1:length(fileList)){
  filename=paste0(
    d,
    fileList[i]
  )
  if(
    grepl(
      pattern="shs",
      x=filename
    )==FALSE
  ){
    if(
      grepl(
        pattern=".xlsx",
        x=filename
      )==TRUE
    ){
      partial=XLConnect::readWorksheetFromFile(
        file=filename,
        sheet=1
      )
    } else {
      if(
        grepl(
          pattern=".csv",
          x=filename
        )==TRUE
      ){
        partial=read.csv(
          file=filename
        )
      }
    }
  } else {
    partial=XLConnect::readWorksheetFromFile(
      file=filename,
      sheet="dealer"
    )
  }
  ## Sometimes the SIMM exports the first column with an extra space in the
  ## column name, so check for that and correct it if it exists
  colnames(partial)[1]=ifelse(
    grepl(
      pattern="Sector.Id",
      x=colnames(partial)[1]
    ),
    "Sector.Id",
    "ERROR"
  )
  Dealer=rbind(Dealer,partial)
}
iDealer=Dealer[,c(
  "Mri",
  "Vessel.Permit.No",
  "Vessel.Name",
  "Vessel.Reg.No",
  "Vtr.Serial.No",
  "State.Land",
  "Port.Land",
  "Species.Itis",
  "Landed.Weight",
  "Live.Weight",
  "Date.Sold"
)]
## Permit numbers should be character strings to maintain leading zeros
iDealer$PERMIT=as.character(
  iDealer$Vessel.Permit.No
)
## Vessel names should be all caps
iDealer$VESSEL=toupper(
  as.character(
    iDealer$Vessel.Name
  )
)
## VTR numbers should be character strings to maintain leading zeros
iDealer$VTR=as.character(
  iDealer$Vtr.Serial.No
)
## Species names can be converted directly frim ITIS numbers
iDealer$SPECIES=NA
for(i in 1:nrow(iDealer)){
  itis=iDealer$Species.Itis[i]
  iDealer$SPECIES[i]=as.character(
    unique(
      species[
        which(
          species$ITIS==itis
        ),
        "AFS"
        ]
    )
  )
}
## Live weights should be reported as numbers
iDealer$WEIGHT=as.numeric(
  as.character(
    iDealer$Live.Weight
  )
)
## Dates should be converted to POSIX values
iDealer$DATE=ymd(
  as.character(
    iDealer$Date.Sold
  )
)