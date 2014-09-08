setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/EventSummaryPlots.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");


###################################################################
## Event Details
###################################################################

eventLat <- 34.51828384;
eventLong <- 69.18315125;
eventType <- "Bombing";

numTowers <- 60;

year <- 11;
month <- 12;
day <- 6;
hour <- 11;
minute <- 52;
second <- 0;
startHour <- 10;
endHour <- 14;

hmRadius <- 30;
hmGeoRadius <- .05;


###################################################################
## Setup variables, create directory
###################################################################
towerLocs <- TowerLocations();

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);

towerId <- eventTowers$TowerId[1];
towerDist <- eventTowers$Dist[1];
eventTime <- second + 60*minute + 60*60*hour;
plotDir <- paste("C:/Plots/NetworkResearch/Event/Summaries/", eventType, "-20",
            year, "-", Pad2(month), "-", Pad2(day), sep="");
plotPrefix <- paste(eventType, "-20", year, "-", Pad2(month), "-", Pad2(day), sep="");

dir.create(plotDir, showWarnings=FALSE, recursive=TRUE);

###################################################################
## Plot volume for the month
###################################################################

setwd(plotDir);
pdf( paste(plotPrefix, "-NearestTowerMonthVolume.pdf", sep=""), height=10, width=10 );
print(PlotMonthCallVolume(towerId, year, month, interestingDay = day));
dev.off();

###################################################################
## 3-day volume around the event at the neares tower
###################################################################

setwd(plotDir);
ggsave( paste(plotPrefix, "-ClosestTowerVolume.pdf", sep=""), height=7, width=10,
       plot=Event3DayVolume(year, month, day, hour, minute, towerId, towerDist) );

###################################################################
## Call Volume vs Distance Heatmaps
###################################################################

towerCalls <- list(incoming=GetTowerInCalls( towerId, year, month ),
                       outgoing=GetTowerOutCalls( towerId, year, month ));

towerLat <- towerLocs$Lat[towerLocs$TowerId == towerId];
towerLong <- towerLocs$Long[towerLocs$TowerId == towerId];

nearTowers <- TowersByDist(towerLat, towerLong,
                           towerLocs);

setwd(plotDir);
ggsave(paste(plotPrefix, "-IncomingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=EventHeatmap(year, month, day, hour, minute, startHour, endHour,
         towerCalls, nearTowers, "Incoming", "Physical", hmRadius));

ggsave(paste(plotPrefix, "-IncomingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=EventHeatmap(year, month, day, hour, minute, startHour, endHour,
         towerCalls, nearTowers, "Incoming", "Geodesic", hmGeoRadius));

ggsave(paste(plotPrefix, "-OutgoingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=EventHeatmap(year, month, day, hour, minute, startHour, endHour,
         towerCalls, nearTowers, "Outgoing", "Physical", hmRadius));

ggsave(paste(plotPrefix, "-OutgoingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=EventHeatmap(year, month, day, hour, minute, startHour, endHour,
         towerCalls, nearTowers, "Outgoing", "Geodesic", hmGeoRadius));

###################################################################
## Standardized call volume for towers near the event
###################################################################

setwd(plotDir);
ggsave(paste(plotPrefix, "-TowerStdVolVsDist.pdf", sep=""), height=10, width=10,
       plot=EventCallVolume(year, month, day, hour, minute, startHour, endHour,
         eventLat, eventLong, numTowers, towerLocs));

