## Script for plotting volume of the nearest tower ids to a given lat and long
## Arguments should be:
##   Latitude
##   Longitude
##   Number of Towers
##   Year (YY)
##   Month (MM)
##   Day (dd)
##   Hour (hh)
##   Minute (mm)
##   EventTitle
##   TowerFile (or blank to use default)
## Example call: Rscript PlotNearestTowers3DayVolume.r 34.518 69.183 10 11 12 06 11 52 Bombing

args <- commandArgs(trailingOnly = TRUE);
lat <- as.numeric(args[1]);
long <- as.numeric(args[2]);
numTowers <- as.numeric(args[3]);
year <- as.numeric(args[4]);
month <- as.numeric(args[5]);
day <- as.numeric(args[6]);
hour <- as.numeric(args[7]);
minute <- as.numeric(args[8]);
eventTitle <- args[9];
towerFile <- args[10];

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Scripts/Plotting");
source("PlotEvent3DayVolume.r");

towers <- NULL;
if ( is.na(towerFile) ) {
    towers <- TowersByDist( lat, long, TowerLocations() );
} else {
    towers <- TowersByDist( lat, long, TowerLocations(towerFile) );
}


for ( i in 1:numTowers ) {
##    print(paste("Tower ", towers$TowerId[i], ", Distance ", round(towers$Dist[i], 2), sep=""));
    PlotEvent3DayVolume( towers$TowerId[i], year, month, day, hour,
                         minute, eventTitle, towers$Dist[i] );
}
