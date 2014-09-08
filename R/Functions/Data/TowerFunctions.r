## File that contains functions related to tower information

currdir <- getwd();
setwd("C:/Code/NetworkResearch/R/Functions");
source("Utilities.r");
source("Mapping/MapFunctions.r");
setwd(currdir);

## Gets a file of towers with their locations
## These towers are renumbered
## The fields are:
##   Tower Id
##   Longitude
##   Latitude
##   TowersAtLocation (tower ids from the raw data) separated by :
TowerLocations <- function(file="C:/Data/NetworkResearch/Towers/towerLoc-2012-10.csv") {
    data <- read.csv(file, header=FALSE);
    colnames(data) <- c("TowerId", "Long", "Lat", "TowerList");
    data;
}

## Sorts the towers by distance from a given location
TowersByDist <- function( lat, long, towerLocs ) {
    data <- towerLocs;
    data$Dist <- apply(data[,c(2,3)], 1, function(loc) {
        kmDist(lat, long, loc[2], loc[1]);
    });
    data[order(data$Dist),];
}

## Gets the incoming calls to a particular tower
## for the given year and month
## Year should be two digits (i.e. 12, not 2012)
GetTowerInCalls <- function( towerId, year, month ) {
    currdir <- getwd();
    setwd(paste("C:/Data/NetworkResearch/TowerCalls/20",
                year, "-", Pad2(month), sep=""));
    fileName <- paste("20", year, "-", Pad2(month),
                      "-Tower", towerId, "In.txt", sep="");
    if ( file.exists(fileName) && file.info(fileName)$size > 0 ) {
      towerData <- read.table(fileName, header=FALSE, sep="|");
      colnames(towerData) <- c("Year", "Month", "Day", "Hour", "Minute",
                               "Second", "FromTowerId", "ToTowerId", "Duration")
      towerData;
    } else { NULL; }
}

## Gets the outgoing calls from a particular tower
## for the given year and month
## Year should be two digits (i.e. 12, not 2012)
GetTowerOutCalls <- function( towerId, year, month ) {
    currdir <- getwd();
    setwd(paste("C:/Data/NetworkResearch/TowerCalls/20",
                year, "-", Pad2(month), sep=""));
    fileName <- paste("20", year, "-", Pad2(month),
                      "-Tower", towerId, "Out.txt", sep="");
    if ( file.exists(fileName) && file.info(fileName)$size > 0 ) {
      towerData <- read.table(fileName, header=FALSE, sep="|");
      colnames(towerData) <- c("Year", "Month", "Day", "Hour", "Minute",
                               "Second", "FromTowerId", "ToTowerId", "Duration")
      towerData;
    } else { NULL; }
}

## Gets the incoming calls to towers in a region
## for the given year and month
## Year should be two digits (i.e. 12, not 2012)
GetRadiusInCalls <- function( lat, long, radius, towerLocs, year, month ) {
  towerDist <- TowersByDist(lat, long, towerLocs);
  calls <- NULL;
  for ( towerId in towerDist$TowerId[towerDist$Dist <= radius] ) {
    towerCalls <- GetTowerInCalls(towerId, year, month);
    if (!is.null(towerCalls)) {
      calls <- rbind(calls, towerCalls);
    }
  }
  calls;
}

## Gets the outgoing calls from towers in a region
## for the given year and month
## Year should be two digits (i.e. 12, not 2012)
GetRadiusOutCalls <- function( lat, long, radius, towerLocs, year, month ) {
  towerDist <- TowersByDist(lat, long, towerLocs);
  calls <- NULL;
  for ( towerId in towerDist$TowerId[towerDist$Dist <= radius] ) {
    towerCalls <- GetTowerOutCalls(towerId, year, month);
    if (!is.null(towerCalls)) {
      calls <- rbind(calls, towerCalls);
    }
  }
  calls;
}

## Function to aggregate the number of calls by period
## calls should be a data frame with data from a single month
## and columns (at least) Day, Hour, Minute, Second
## and a single row for each call (Output from GetTower<In/Out>Calls)
AggregateCalls <- function(calls, periodLength) {
    calls$DaySec <- calls$Second + 60*calls$Minute + 3600*calls$Hour;
    calls$DayPeriod <- floor(calls$DaySec / periodLength);

    nPeriods <- 24*60*60/periodLength;
    calls$Period <- nPeriods * (calls$Day-1) + calls$DayPeriod;
    periodCalls <- table(calls$Period);

    nDays <- max(calls$Day);
    periodCalls.df <- data.frame( Day=sort(rep(1:nDays, 24*60*60/periodLength)),
                                  DayPeriod=0:(nPeriods-1),
                                  Period=0:(nDays*nPeriods-1),
                                  Calls=0 );
    periodCalls.df$Calls[as.numeric(names(periodCalls))+1] <- periodCalls;
    periodCalls.df;
}

## Calculates the average for each period by using the same
## period in every other week in the data
## calls should be a data frame with columns Day, DayPeriod and Calls
CalculateWeeklyAverages <- function(calls) {
    sapply( 1:nrow(calls), function(i) {
        day <- calls$Day[i];
        period <- calls$DayPeriod[i];
        mean(calls$Calls[calls$Day != day &
                         calls$Day%%7 == day%%7 &
                         calls$DayPeriod == period]);
    } );
}

## Gets the aggregated calls for a tower, year and month
## Right now, just gets the 5 minute agggregated data
GetTowerAggregatedCalls <- function( towerId, year, month ) {
    currdir <- getwd();
    setwd(paste("C:/Data/NetworkResearch/TowerCalls/20",
                year, "-", Pad2(month), "/Agg5min", sep=""));
    if (file.exists(paste("20", year, "-", Pad2(month),
                          "-Tower", towerId, "In-5min.txt", sep=""))) {
        inCalls <- read.table(paste("20", year, "-", Pad2(month),
                                    "-Tower", towerId, "In-5min.txt", sep=""),
                              header=FALSE, sep="|",
                              col.names=c("Day", "DayPeriod", "Period",
                                  "InCalls", "InAvgCalls"));
        outCalls <- read.table(paste("20", year, "-", Pad2(month),
                                     "-Tower", towerId, "Out-5min.txt", sep=""),
                               header=FALSE, sep="|",
                               col.names=c("Day", "DayPeriod", "Period",
                                   "OutCalls", "OutAvgCalls"));
        setwd(currdir);
        minRow <- min(nrow(inCalls), nrow(outCalls));
        inCalls <- inCalls[1:minRow,];
        outCalls <- outCalls[1:minRow,];
        inCalls$OutCalls <- outCalls$OutCalls;
        inCalls$OutAvgCalls <- outCalls$OutAvgCalls;
        inCalls$TotalCalls <- inCalls$InCalls + inCalls$OutCalls;
        inCalls$TotalAvgCalls <- inCalls$InAvgCalls + inCalls$OutAvgCalls;
        inCalls$InDiffCalls <- inCalls$InCalls - inCalls$InAvgCalls;
        inCalls$OutDiffCalls <- inCalls$OutCalls - inCalls$OutAvgCalls;
        inCalls$TotalDiffCalls <- inCalls$TotalCalls - inCalls$TotalAvgCalls;
        inCalls <- inCalls[order(inCalls$Period),];
        inCalls;
    } else {
        setwd(currdir);
        NULL;
    }
}

## Gets the aggregated calls for all towers in a region
GetRadiusAggregatedCalls <- function( lat, long, radius, towerLocs, year, month ) {
  towerDist <- TowersByDist(lat, long, towerLocs);
  towerIds <- towerDist$TowerId[towerDist$Dist <= radius];
  calls <- NULL;
  if ( length(towerIds) > 0 ) {
    calls <- GetTowerAggregatedCalls(towerDist$TowerId[1], year, month);
    for ( towerId in towerIds[-1] ) {
      towerCalls <- GetTowerAggregatedCalls(towerId, year, month);
      if (!is.null(towerCalls) && nrow(towerCalls) == nrow(calls)) {
        calls$InCalls <- calls$InCalls + towerCalls$InCalls;
        calls$InAvgCalls <- calls$InAvgCalls + towerCalls$InAvgCalls;
        calls$OutCalls <- calls$OutCalls + towerCalls$OutCalls;
        calls$OutAvgCalls <- calls$OutAvgCalls + towerCalls$OutAvgCalls;
        calls$TotalCalls <- calls$TotalCalls + towerCalls$TotalCalls;
        calls$TotalAvgCalls <- calls$TotalAvgCalls + towerCalls$TotalAvgCalls;
        calls$InDiffCalls <- calls$InDiffCalls + towerCalls$InDiffCalls;
        calls$OutDiffCalls <- calls$OutDiffCalls + towerCalls$OutDiffCalls;
        calls$TotalDiffCalls <- calls$TotalDiffCalls + towerCalls$TotalDiffCalls;
      }
    }
  }
  calls;
}

GetNetworkSnapshot <- function( year, month, day, startSec, endSec ) {
    currdir <- getwd();
    setwd(paste("C:/Data/NetworkResearch/NetworkSnapshots/20",
                Pad2(year), "-", Pad2(month), "/", Pad2(day), sep=""));
    data <- read.table(paste("20", Pad2(year), "-", Pad2(month), "-",
                             Pad2(day), "-Network-", startSec, "-",
                             endSec, "-Network.txt", sep=""), sep="|");
    setwd(currdir);
    colnames(data) <- c("FromTower", "ToTower", "Calls");
}

GetNetworkSnapshot2 <- function( year, month, day, hour, minute, nMin ) {
    currdir <- getwd();
    setwd(paste("C:/Data/NetworkResearch/NetworkSnapshots/20",
                Pad2(year), "-", Pad2(month), "/", Pad2(day), sep=""));
    data <- read.table(paste("20", Pad2(year), "-", Pad2(month), "-",
                             Pad2(day), "-Network-", Pad2(hour), Pad2(minute),
                             "00-", Pad2(hour), Pad2(minute+nMin-1), "59",
                             "-Network.txt", sep=""), sep="|");
    setwd(currdir);
    colnames(data) <- c("FromTower", "ToTower", "Calls");
    data;
}
