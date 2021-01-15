## ---------------------------
## Script name: eVTRStandardization.R
##
## Purpose of script: The electronic Vessel Trip Report data that is available
##    through the Greater Atlantic Regional Fisheries Office is not directly
##    compatible with EM data or dealer data. This script standardizes the eVTR
##    file format. 
##
## Author: George A. Maynard
##
## Date Created: 2021-01-11
##
## Copyright (c) George A. Maynard, 2021
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
## Notes: There are some eVTR Species_ID codes that are not easily identifiable
##    and throw errors when matched with known species abbreviations / codes. 
##    The current list (which are skipped by this script) includes the following:
##      "DCP","NC","OFF","CRNS"
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
library(stringdist)
##
## ---------------------------
## Load any custom functions
##
## ---------------------------
## The VTR data is provided in a .csv file from GARFO
VTR=read.csv(
  file=file.choose()
)
## ---------------------------
## Standardize species names
## Read in the fuzzy matching file from GitHub
species=read.csv("https://raw.githubusercontent.com/gamaynard/NGA/main/species.csv")
species=read.csv("species.csv")
## Create a new data frame to store standardized values
new=VTR
new$AFS=NA
new$ITIS=NA
sc=which(colnames(new)=='SPECIES_ID')
## Use fuzzy matching to convert each species name into a standardized value
for(i in 1:nrow(new)){
  ## Skip unresolvable species codes
  if(new[i,sc]%in%c("DCP","NC","OFF","CRNS")==FALSE){
    ## calculate the minimum string distance from the existing value
    ## to commonly used, non-standardized values
    b=min(
      stringdist(
        a=toupper(new[i,sc]),
        b=toupper(species$RAW)
      )
    )
    ## assign a standardized value that matches the closest non-standardized
    ## value
    afs=species$AFS[
      which(stringdist(
        a=toupper(new[i,sc]),
        b=toupper(species$RAW)
      )==b
      )
      ]
    itis=species$ITIS[
      which(stringdist(
        a=toupper(new[i,sc]),
        b=toupper(species$RAW)
      )==b
      )
      ]
    ## If there is more than one unique value this portion of
    ## the code will throw an error
    if(length(unique(afs))==1){
      new$AFS[i]=as.character(afs[1])
      new$ITIS[i]=as.character(itis[1])
    } else {
      print(unique(afs))
      warning(paste0('SCRIPT STOPPED ON LINE ',i))
      warning(paste0('SPECIES VALUE: ',new[i,sc]))
      stop('ERROR: MULTIPLE SPECIES MATCHES. SEE ABOVE.')
    }
  }
}
iVTR=new[,c(
  "DATE_SAIL",
  "DATE_LAND",
  "VESSEL_PERMIT_NUM",
  "SERIAL_NUM",
  "GEARCODE",
  "GEARQTY",
  "GEARSIZE",
  "AREA",
  "LAT_DEGREE",
  "LAT_MINUTE",
  "LAT_SECOND",
  "LON_DEGREE",
  "LON_MINUTE",
  "LON_SECOND",
  "NTOWS",
  "DATETIME_HAUL_START",
  "DATETIME_HAUL_END",
  "SPECIES_ID",
  "KEPT",
  "DISCARDED",
  "PORT_LANDED",
  "AFS",
  "ITIS"
)]
colnames(iVTR)=tolower(
  colnames(iVTR)
)
## SAILDATE should be a POSIX value
iVTR$SAILDATE=ymd_hms(
  as.character(iVTR$date_sail)
)
## LANDDATE should be a POSIX value
iVTR$LANDDATE=ymd_hms(
  as.character(iVTR$date_land)
)
## PERMIT should be a character string
iVTR$PERMIT=as.character(
  iVTR$vessel_permit_num
)
## Check for NA values in the seconds columns of both latitude and longitude
## and if they exist, replace them with zeros
iVTR$lat_second=ifelse(
  is.na(iVTR$lat_second),
  0,
  iVTR$lat_second
)
iVTR$lon_second=ifelse(
  is.na(iVTR$lon_second),
  0,
  iVTR$lon_second
)
## All longitude values should be west of the Prime Meridian (negative)
iVTR$lon_degree=ifelse(
  iVTR$lon_degree>0,
  iVTR$lon_degree*-1,
  iVTR$lon_degree
)
## Combine degrees, minutes, and seconds into decimal degrees for both latitude
## and longitude
iVTR$LAT=iVTR$lat_degree+iVTR$lat_minute/60+iVTR$lat_second/(60^2)
iVTR$LON=iVTR$lon_degree-iVTR$lon_minute/60-iVTR$lon_second/(60^2)
## Replace Gear Codes with human-readable values
iVTR$GEAR=ifelse(iVTR$gearcode=="GNS","GILLNET",
  ifelse(iVTR$gearcode=="HND","JIG",
    ifelse(iVTR$gearcode=="LLB","LONGLINE",
      ifelse(iVTR$gearcode=="OTF","TRAWL",
        ifelse(iVTR$gearcode=="PTL","LOBSTER POT",
          iVTR$gearcode
        )
      )
    )
  )
)
## Ensure stat areas are reported as numbers
iVTR$AREA=as.numeric(
  as.character(
    iVTR$area
  )
)
## Trim serial numbers to generate VTR numbers
iVTR$VTR=NA
iVTR$VTR=ifelse(
  nchar(iVTR$serial_num)==16,
  substr(
    iVTR$serial_num,1,14
  ),
  iVTR$VTR
)
## Any trips with paper trip reports only have an 8 character identifier and
## should be excluded from this analysis, as they are likely declared out of
## fishery
iVTR=subset(
  iVTR,
  nchar(iVTR$serial_num)==16
)
## Haul start and end times should be POSIX formatted values
iVTR$HAULSTART=ymd_hms(
  as.character(
    iVTR$datetime_haul_start
  )
)
iVTR$HAULEND=ymd_hms(
  as.character(
    iVTR$datetime_haul_end
  )
)
## Kept and discarded weights should be numeric
iVTR$KEPT=as.numeric(
  as.character(
    iVTR$kept
  )
)
iVTR$DISCARDED=as.numeric(
  as.character(
    iVTR$discarded
  )
)