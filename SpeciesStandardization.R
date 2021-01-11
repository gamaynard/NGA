## ---------------------------
## Script name: SpeciesStandardization.R
##
## Purpose of script: To transform commonly used fish species names, codes, and 
##    abbreviations to standard formats such as American Fisheries Society (AFS)
##    name or International Taxonomic Information System (ITIS) code.  
##
## Author: George A. Maynard
##
## Date Created: 2021-01-11
##
## Copyright (c) George A. Maynard, 2021
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
## Notes: Currently this code is set up for species commonly interacted with by 
##    the New England commercial groundfish fleet. However, the "species.csv" 
##    file used could be expanded or altered to encompass other fisheries.
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
library(stringdist)
##
## ---------------------------
## Load any custom functions
##
## ---------------------------
## Prompt the user for a raw data frame that should contain a column named
## "species".
filename=file.choose()
raw=read.csv(
  file=filename
  )
## If the raw data frame does not have a species column, prompt the
## user to define one.
sc=which(tolower(colnames(raw))=="species")
if(length(sc)==0){
  print(colnames(raw))
  readline("NO SPECIES COLUMN FOUND. Available columns listed above. Press <Enter> to continue.")
  sc=readline(
    prompt="Please enter name of column to be analyzed and press <Enter> when finished: "
  )
  sc=tolower(sc)
  if(sc%in%tolower(colnames(raw))==FALSE){
    stop("SPECIFIED COLUMN NOT FOUND")
  }
  sc=which(tolower(colnames(raw))==sc)
}
## Read in the fuzzy matching file from GitHub
species=read.csv("https://raw.githubusercontent.com/gamaynard/NGA/main/species.csv")
## Create a new data frame to store standardized values
new=raw
new$AFS=NA
new$ITIS=NA
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
## Print out the new dataset (which is the same as the old, with two additional,
##  standardized columns) to a local .csv file
nf=paste0("STANDARDIZED",filename)
write.csv(
  x=new,
  file=nf
)
print(paste0("Your standardized dataset has been printed to: ",nf))