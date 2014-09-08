## Script for getting the nearest tower ids to a given lat and long
## Arguments should be:
##   Latitude
##   Longitude
##   Number of Towers
##   OutputFile
##   TowerFile (or blank to use default)
## Example call: Rscript NearestTowersToLocation.r 34.518 69.183 10 /ischool/jb/roshan_anon/wmchad/Temp/TestTowers2.txt

args <- commandArgs(trailingOnly = TRUE);
lat <- as.numeric(args[1]);
long <- as.numeric(args[2]);
numTowers <- as.numeric(args[3]);
outputFile <- args[4];
towerFile <- args[5];

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Functions");
source("Data/TowerFunctions.r");
source("Mapping/MapFunctions.r");

towers <- NULL;
if ( is.na(towerFile) ) {
    towers <- TowersByDist( lat, long, TowerLocations() );
} else {
    towers <- TowersByDist( lat, long, TowerLocations(towerFile) );
}

write.table( towers$TowerId[1:numTowers], outputFile, quote=F, row.names=F, col.names=F );
