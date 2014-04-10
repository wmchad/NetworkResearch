## converts a file from a list of matched tower ids to a tower map file

setwd("C:/Data/NetworkResearch/Towers");

towerLocations <- read.csv("towerLoc-2012-10.csv", header=FALSE, stringsAsFactors=FALSE);
colnames(towerLocations) <- c("Id", "Long", "Lat", "Towers");

towerMap <- data.frame(TowerId=0, Id=0);

insertIndex <- 1;

for ( i in 1:nrow(towerLocations) ) {
  id <- towerLocations$Id[i];
  towers <- strsplit(towerLocations$Towers[i], ":")[[1]];
  for ( j in 1:length(towers) ) {
    towerMap[insertIndex,] <- c(towers[j], id);
    insertIndex <- insertIndex + 1;
  }
}

write.table( towerMap, "towerMap-2012-10.txt", sep=",",
             quote=FALSE, row.names=FALSE, col.names=FALSE );
