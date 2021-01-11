## ---------------------------
## Script name: EMStandardization.R
##
## Purpose of script: The NOAA Electronic Monitoring API accepts data as .json
##    files. This file takes a .json file and converts it into a .csv flat file
##    that is standardized and easily comparable with .csv files downloaded from
##    the SIMM and from GARFO's databases. 
##
## Author: George A. Maynard
##
## Date Created: 2021-01-11
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
library(RJSONIO)
library(stringdist)
##
## ---------------------------
## Load any custom functions
##
## ---------------------------
## Read in EM data and transform it into a flat file
## EM data is read in from a .json file which functions as a list of lists
EM_JSON=fromJSON(
  content=file.choose()
)
## Create a new empty EM data frame to store data as a flat file
EM=data.frame(
  VTR=as.numeric(),
  VESSEL=as.character(),
  HAUL_NO=as.numeric(),
  startTime=as.numeric(),
  endTime=as.numeric(),
  startLat=as.numeric(),
  startLon=as.numeric(),
  species=as.character(),
  count=as.numeric(),
  weight=as.numeric()
)
## Each trip is an item in a list
## For each trip
for(i in 1:length(EM_JSON)){
  ## Open the list item
  trip=EM_JSON[[i]]
  ## Each trip is a list of variables and dataframes
  ## Extract the VTR number
  vtr=trip$trip_id
  ## Extract the vessel name
  vessel=trip$vessel_name
  ## Extract the number of hauls
  hauls=trip$total_hauls
  ## Report discards haul by haul
  for(h in 1:hauls){
    haul=trip$hauls[[h]]
    startTime=haul[2]
    endTime=haul[3]
    startLat=haul[4]
    startLon=haul[5]
    ## Discards are listed by species
    discards=haul$discards
    for(d in 1:length(discards)){
      species=discards[[d]]$species
      count=discards[[d]]$count_discarded
      weight=discards[[d]]$pounds_discarded
      newline=data.frame(
        VTR=as.numeric(),
        VESSEL=as.character(),
        HAUL_NO=as.numeric(),
        startTime=as.numeric(),
        endTime=as.numeric(),
        startLat=as.numeric(),
        startLon=as.numeric(),
        species=as.character(),
        count=as.numeric(),
        weight=as.numeric()
      )
      newline[1,]=NA
      newline$VESSEL=as.character(newline$VESSEL)
      newline$species=as.character(newline$species)
      newline$VTR=vtr
      newline$VESSEL=toupper(
        as.character(vessel)
      )
      newline$HAUL_NO=h
      newline$startTime=startTime
      newline$endTime=endTime
      newline$startLat=startLat
      newline$startLon=startLon
      newline$species=toupper(
        as.character(species)
      )
      newline$count=count
      newline$weight=weight
      EM=rbind(EM,newline)
      rm(newline)
    }
  }
}
## ---------------------------
## Standardize species names
## Read in the fuzzy matching file from GitHub
species=read.csv("https://raw.githubusercontent.com/gamaynard/NGA/main/species.csv")
## Create a new data frame to store standardized values
new=EM
new$AFS=NA
new$ITIS=NA
sc=which(colnames(new)=='species')
## Use fuzzy matching to convert each species name into a standardized value
for(i in 1:nrow(new)){
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
    warning(paste0('SPECIES VALUE: ',raw[i,sc]))
    stop('ERROR: MULTIPLE SPECIES MATCHES. SEE ABOVE.')
  }
}
## ---------------------------
## The VTR column is already a character vector (to avoid loss of leading zeros)
## The VESSEL column is already a character vector
## The HAUL_NO column is already an integer
## The startTime column needs to be converted to a POSIX value
EM$STARTTIME=ymd_hm(
  as.character(
    EM$startTime
  )
)
## The endTime column needs to be converted to a POSIX value
EM$ENDTIME=ymd_hm(
  as.character(
    EM$endTime
  )
)
## The startLat column needs to be converted to a number
EM$STARTLAT=as.numeric(
  as.character(
    EM$startLat
  )
)
## The startLon column needs to be converted to a number
EM$STARTLON=as.numeric(
  as.character(
    EM$startLon
  )
)
## Discard Count needs to be a number
EM$DiscardCount=as.numeric(
  as.character(
    EM$count
  )
)
## DiscardWeight needs to be a number
EM$DiscardWeight=as.numeric(
  as.character(
    EM$weight
  )
)