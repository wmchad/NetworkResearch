require(reshape);
setwd("C:/Data/NetworkResearch/NetworkSnapshots");


daySnapshot <- read.table( "2011-11-05-000000-235959-Network.txt", sep="|" );
colnames(daySnapshot) <- c("FromTower", "ToTower", "CallCount");

daySnapshot <- daySnapshot[order( daySnapshot$FromTower, daySnapshot$ToTower ),];

plot(daySnapshot$ToTower, daySnapshot$FromTower, pch=".");

hist(log10(daySnapshot$CallCount));

sum(sort(unique(daySnapshot$ToTower)) == sort(unique(daySnapshot$FromTower)))

setwd("C:/Data/NetworkResearch/Towers");

towerLocations <- read.csv("towerLoc-2012-10.csv", header=FALSE, stringsAsFactors=FALSE);
colnames(towerLocations) <- c("Id", "Long", "Lat", "Towers");

setwd("~/Dropbox/Chad/UW/NetworkResearch/Code/R/Functions/");
source("DataSets.r");

towerLocations$Dist <- apply(towerLocations[,c(2,3)], 1, function(loc) {
  kmDist(34.5, 69.2, loc[2], loc[1]);
})

towerLocations <- towerLocations[order(towerLocations$Dist),];

towerLocations$newId <- 1:nrow(towerLocations);

daySnapshot.try <- data.frame(t(apply(daySnapshot, 1, function(rec) {
  newFromId <- towerLocations$newId[towerLocations$Id==rec[[1]]];
  if ( length(newFromId) == 0 ) {
    newFromId <- rec[[1]];
  }
  newToId <- towerLocations$newId[towerLocations$Id==rec[[2]]];
  if ( length(newToId) == 0 ) {
    newToId <- rec[[2]];
  }
  c(rec[[1]], rec[[2]], rec[[3]], newFromId, newToId);
})));

colnames(daySnapshot.try) <- c("FromTower", "ToTower", "CallCount", "NewFromTower", "NewToTower");
plot(daySnapshot.try$NewToTower, daySnapshot.try$NewFromTower, pch=".");

hist(log10(daySnapshot.try$CallCount));

max(daySnapshot.try$CallCount);
# 18354
