# NGA
Code for translating between different fishery dependent data formats identified during the course of the Cape Cod Commercial Fishermen's Alliance Data Portal Project, funded by the Net Gains Alliance.

All of the data sets referenced within these scripts have been catalogued in a metadata archive. To request access to the archive, please contact [Melissa Sanderson](mailto:melissa@capecodfishermen.org) at the [Cape Cod Commercial Fishermen's Alliance](https://www.capecodfishermen.org/). 

## [SpeciesStandardization.R](SpeciesStandardization.R)
This script takes a .csv file that has a species column and compares it to a long list of permutations of species names for fish and invertebrates commonly caught by the New England commercial groundfish fleet. Using fuzzy matching, each of the non-standardized entries is assigned a standardized American Fisheries Society common name as well as an Interagency Taxonomic Information System (ITIS) identification code. These two naming conventions adhere to the Atlantic Coastal Cooperative Statistics Program standards for fisheries data on the east coast of the United States. The results of the script are exported as a .csv file with a "STANDARDIZED" tag in the file name. 
