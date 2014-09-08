require("reshape");
require("grid");
require("Matrix");

setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");

CombineSnapshots <- function( year, month, day, hour, minute, nMin, nPeriods ) {
  X <- matrix(nrow=1139, ncol=1139, data=0);
  for ( p in 1:nPeriods ) {
    snap <- GetNetworkSnapshot2( year, month, day, hour, minute, nMin );
    for ( i in 1:nrow(snap) ) {
      if ( snap$FromTower[i] > 0 & snap$FromTower[i] <= 1139 &
          snap$ToTower[i] > 0 & snap$ToTower[i] <= 1139 ) {
        X[snap$FromTower[i], snap$ToTower[i]] <- X[snap$FromTower[i], snap$ToTower[i]] +
          snap$Calls[i];
      }
    }
    minute <- minute+nMin;
    if ( minute >= 60 ) {
      minute <- minute - 60;
      hour <- hour + 1;
    }
  }
  X;
}

AverageSymmetricSnapshot <- function( year, months, days, hour, minute,
                                nMin, nPeriods, nTowers ) {
  snapshot <- matrix(nrow=nTowers, ncol=nTowers, data=0);
  for ( i in 1:length(months) ) {
##    print(paste(months[i], days[i]));
    snapshot <- snapshot + CombineSnapshots( year, months[i], days[i], hour, minute, nMin, nPeriods );
  }
  snapshot <- snapshot + t(snapshot);
  diag(snapshot) <- diag(snapshot)/2;
  snapshot / length(months);
}

months <- c(rep(11, 14), rep(12, 2));
days <- c(8:10, 14:17, 21:24, 28:30, 1, 5);

for ( hr in 6:19 ) {
  for ( min in c(0, 30) ) {
    avgSnapshot <- AverageSymmetricSnapshot( 11, months, days, hr, min, 30, 1, 1139 );
    avgDf <- melt(avgSnapshot);
    avgDf <- avgDf[avgDf$value != 0,];
    snapFile <- paste("2011-12-06-AvgNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-12/06");
    write.table(avgDf, snapFile, quote=FALSE, row.names=FALSE);
  }
}


for ( hr in 6:19 ) {
  for ( min in c(0, 30) ) {
    avgFile <- paste("2011-12-06-AvgNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-12/06");
    avgDf <- read.table(avgFile, header=TRUE);
    daySnapshot <- AverageSymmetricSnapshot( 11, 12, 6, hr, min, 30, 1, 1139 );
    for ( i in 1:nrow(avgDf) ) {
      daySnapshot[avgDf$X1[i], avgDf$X2[i]] <-
        daySnapshot[avgDf$X2[i], avgDf$X1[i]] <-
          daySnapshot[avgDf$X1[i], avgDf$X2[i]] - avgDf$value[i];
    }
    diffDf <- melt(daySnapshot);
    diffDf <- diffDf[diffDf$value != 0,];
    diffFile <- paste("2011-12-06-DetrendedNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-12/06");
    write.table(diffDf, diffFile, quote=FALSE, row.names=FALSE);
  }
}

months <- c(6,7,7,7);
days <- c(25,2,9,16);

for ( hr in 6:19 ) {
  for ( min in c(0, 30) ) {
    avgSnapshot <- AverageSymmetricSnapshot( 11, months, days, hr, min, 30, 1, 1139 );
    avgDf <- melt(avgSnapshot);
    avgDf <- avgDf[avgDf$value != 0,];
    snapFile <- paste("2011-07-23-AvgNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-07/23");
    write.table(avgDf, snapFile, quote=FALSE, row.names=FALSE);
  }
}


for ( hr in 6:19 ) {
  for ( min in c(0, 30) ) {
    avgFile <- paste("2011-07-23-AvgNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-07/23");
    avgDf <- read.table(avgFile, header=TRUE);
    daySnapshot <- AverageSymmetricSnapshot( 11, 7, 23, hr, min, 30, 1, 1139 );
    for ( i in 1:nrow(avgDf) ) {
      daySnapshot[avgDf$X1[i], avgDf$X2[i]] <-
        daySnapshot[avgDf$X2[i], avgDf$X1[i]] <-
          daySnapshot[avgDf$X1[i], avgDf$X2[i]] - avgDf$value[i];
    }
    diffDf <- melt(daySnapshot);
    diffDf <- diffDf[diffDf$value != 0,];
    diffFile <- paste("2011-07-23-DetrendedNetwork-", Pad2(hr), Pad2(min), ".txt", sep="");
    setwd("c:/Data/NetworkResearch/NetworkSnapshots/2011-07/23");
    write.table(diffDf, diffFile, quote=FALSE, row.names=FALSE);
  }
}
