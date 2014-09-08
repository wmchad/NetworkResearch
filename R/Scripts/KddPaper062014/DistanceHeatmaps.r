require("ggplot2");
require("reshape");
require("grid");

setwd("c:/Code/NetworkResearch/R/Functions/Data");
source("TowerFunctions.r");

TimeAxis <- function() {
  xpts <- c(7, 13, 19, 25, 31, 37, 43);
  xlabs <- c( "10:30 AM", "11:00 AM", "11:30 AM", "12:00 PM", "12:30 PM",
              "1:00 PM", "1:30 PM" );
  scale_x_continuous( breaks=xpts, labels=xlabs );
}

TimeAxis2 <- function() {
  xpts <- c(7, 13, 19, 25, 31, 37);
  xlabs <- c( "7:30 AM", "8:00 AM", "8:30 AM", "9:00 AM", "9:30 AM", "10:00 AM" );
  scale_x_continuous( breaks=xpts, labels=xlabs );
}

TimeAxis3 <- function() {
  xpts <- seq(from=1, to=97, by=12);
  xlabs <- c( "11:00 AM", "12:00 PM", "1:00 PM",
              "2:00 PM", "3:00 PM", "4:00 PM",
              "5:00 PM", "6:00 PM", "7:00 PM");
  scale_x_continuous( breaks=xpts, labels=xlabs );
}

TowerDistancesPlot <- function(towerId, towerLocs, subLocs,
                               dmfResult, eventIndex, xAxis,
                               xLab="xxx", yLab="yyy",
                               plotTitle="Title") {
  towerLat <- towerLocs$Lat[towerId];
  towerLong <- towerLocs$Long[towerId];

  eventTowers <- TowersByDist(towerLat, towerLong, subLocs)[-1,];

  subL <- dmfResult$L[eventTowers$TowerId,,];

  towerL <- dmfResult$L[towerId,,];

  T <- ncol(towerL);

  towerDistances <- matrix(data=0, nrow=nrow(eventTowers), ncol=T);

  for (t in 1:T) {
    for ( i in 1:nrow(eventTowers) ) {
      towerDistances[i,t] <- sqrt(sum((towerL[,t] - subL[i,,t])^2));
    }
  }

  dfDist <- melt(towerDistances);
  colnames(dfDist) <- c("Tower", "Time", "Distance");
  dfDist$Tower <- dfDist$Tower;
#  print(paste("Max Distance:", max(dfDist$Distance)));
#  print(paste("Min Distance:", min(dfDist$Distance)));
  
  ggplot(dfDist, aes(Time, Tower, fill=Distance)) + geom_raster() +
    scale_fill_gradient2(low="grey", mid="darkblue", high="blue",
                         midpoint=13, guide=FALSE, limits=c(0, 26)) + xAxis +
      annotate("point", x=eventIndex, y=-14, color="green", size=7) +
        theme(text=element_text(size=20), axis.ticks.y=element_blank(),
              axis.text.y=element_blank()) +
          labs(x=xLab, y=yLab, title=plotTitle);
}

CombineSnapshots <- function( year, month, day, hour, minute, nMin, nPeriods ) {
  X <- matrix(nrow=1139, ncol=1139, data=0);
  for ( p in 1:nPeriods ) {
    snap <- GetNetworkSnapshot2( year, month, day, hour, minute, nMin );
    snap <- snap[!is.na(snap$FromTower) & !is.na(snap$ToTower),];
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

BuildSnapshotArray <- function( year, month, day, hour, minute,
                                nMin, nPeriods, nSnapshots, nTowers ) {
  Xarray <- array(data=0, dim=c(nTowers, nTowers, nSnapshots));
  for ( s in 1:nSnapshots ) {
    hr <- hour + floor((minute+(s-1)*nMin*nPeriods)/60);
    min <- (minute+(s-1)*nMin*nPeriods) %% 60;
    Xarray[,,s] <- CombineSnapshots( year, month, day, hr, min, nMin, nPeriods );
  }
  Xarray;
}

setwd("c:/Data/NetworkResearch/Results/");
load("bombTMF.rdata");

bombNetData <- BuildSnapshotArray( 11, 12, 6, 10, 0, 5, 1, 48, 1139 );

towerCalls <- apply(bombNetData, 1, sum);

highTrafficTowers <- (1:1139)[towerCalls >= 1000];

towerLocs <- TowerLocations();

subLocs <- towerLocs[highTrafficTowers,];

TowerDistancesPlot(790, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
                   xLab="", yLab="", plotTitle="Closest Tower"); #closest
TowerDistancesPlot(405, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 2nd closest
TowerDistancesPlot(683, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 10 km
TowerDistancesPlot(11, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 102 km
TowerDistancesPlot(337, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 166 km
TowerDistancesPlot(640, towerLocs, subLocs, bombTMF, 22, TimeAxis()); # 208 km

setwd("c:/Data/NetworkResearch/Results/");
ggsave("bombDistMatrixClosest.png", height=8, width=10,
       plot=TowerDistancesPlot(790, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
         xLab="", yLab="", plotTitle=""));
ggsave("bombDistMatrix102km.png", height=8, width=10,
       plot=TowerDistancesPlot(11, towerLocs, subLocs, bombTMF, 22, TimeAxis()));
ggsave("bombDistMatrix166km.png", height=8, width=10,
       plot=TowerDistancesPlot(337, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
         xLab="", yLab="", plotTitle=""));
ggsave("bombDistMatrix208km.png", height=8, width=10,
       plot=TowerDistancesPlot(640, towerLocs, subLocs, bombTMF, 22, TimeAxis()));


setwd("c:/Data/Temp/");
load("inaugTMF.rdata");

inaugNetData <- BuildSnapshotArray( 11, 12, 15, 7, 0, 5, 1, 36, 1139 );

towerCalls2 <- apply(inaugNetData, 1, sum);

highTrafficTowers2 <- (1:1139)[towerCalls2 >= 1000];

subLocs2 <- towerLocs[highTrafficTowers2,];

TowerDistancesPlot(337, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()); # closest
TowerDistancesPlot(633, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()); # 2nd closest
TowerDistancesPlot(1041, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()); # 70 km
TowerDistancesPlot(790, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()); # 166 km
TowerDistancesPlot(329, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()); # 208 km

setwd("c:/Data/NetworkResearch/Results/");
ggsave("inaugDistMatrixClosest.png", height=8, width=10,
       plot=TowerDistancesPlot(337, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()));
ggsave("inaugDistMatrix70km.png", height=8, width=10,
       plot=TowerDistancesPlot(1041, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()));
ggsave("inaugDistMatrix166km.png", height=8, width=10,
       plot=TowerDistancesPlot(790, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()));
ggsave("inaugDistMatrix208km.png", height=8, width=10,
       plot=TowerDistancesPlot(329, towerLocs, subLocs2, inaugTMF, 7, TimeAxis2()));



setwd("c:/Data/NetworkResearch/Results");
load("inaugDMF.rdata");

event3Lat <- 34.283;
event3Long <- 70.653;



inaug2NetData <- BuildSnapshotArray( 11, 7, 23, 11, 0, 5, 1, 108, 1139 );

towerCalls3 <- apply(inaug2NetData, 1, sum);

highTrafficTowers3 <- (1:1139)[towerCalls3 >= 2160];

subLocs3 <- towerLocs[highTrafficTowers3,];

event3Towers <- TowersByDist(event3Lat, event3Long, subLocs3);

TowerDistancesPlot(261, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # closest
TowerDistancesPlot(125, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 2nd closest (14 km)
TowerDistancesPlot(874, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); #  124 km
TowerDistancesPlot(92, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 169 km
TowerDistancesPlot(790, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3()); # 137 km

setwd("c:/Data/NetworkResearch/Results/");
ggsave("inaug2DistMatrixClosest.png", height=8, width=10,
       plot=TowerDistancesPlot(261, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3(),
         xLab="", yLab="", plotTitle=""));
ggsave("inaug2DistMatrix169km.png", height=8, width=10,
       plot=TowerDistancesPlot(92, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3(),
         xLab="", yLab="", plotTitle=""));





plot1 <- TowerDistancesPlot(790, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
                            xLab="", yLab="", plotTitle=""); #closest
plot3 <- TowerDistancesPlot(337, towerLocs, subLocs, bombTMF, 22, TimeAxis(),
                            xLab="", yLab="", plotTitle=""); # 166 km
plot2 <- TowerDistancesPlot(261, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3(),
                            xLab="", yLab="", plotTitle=""); #closest
plot4 <- TowerDistancesPlot(92, towerLocs, subLocs3, inaugTMF, 31, TimeAxis3(),
                            xLab="", yLab="", plotTitle=""); # 169 km

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
png("MultiDistMatrix.png", height=1200, width=1500);
print(multiplot( plot1, plot2, plot3, plot4, cols=2 ));
dev.off();
