require(ggplot2);

setwd("C:/Code/NetworkResearch/R/Functions/Data");
source("TowerFunctions.r");

towerLocs <- TowerLocations();

nearTowers <- TowersByDist(34.283192, 70.652997,
                           TowerLocations());

nearestTowerId <- nearTowers$TowerId[1];
## nearestTowerId <- 281

nearTowerCalls <- list(incoming=GetTowerInCalls( nearestTowerId, 11, 7 ),
                       outgoing=GetTowerOutCalls( nearestTowerId, 11, 7 ));

nearTowerCalls$incoming <- nearTowerCalls$incoming[nearTowerCalls$incoming$Day==23,];
nearTowerCalls$incoming$Period <- nearTowerCalls$incoming$Hour*4 +
  floor(nearTowerCalls$incoming$Minute/15);
incomingData <- nearTowerCalls$incoming[,c("FromTowerId", "Period")];
colnames(incomingData) <- c("TowerId", "Period");
incomingData <- merge( incomingData, nearTowers, by="TowerId" )
incomingData <- incomingData[,c("TowerId", "Period", "Dist")];

nearTowerCalls$outgoing <- nearTowerCalls$outgoing[nearTowerCalls$outgoing$Day==23,];
nearTowerCalls$outgoing$Period <- nearTowerCalls$outgoing$Hour*4 +
  floor(nearTowerCalls$outgoing$Minute/15);
outgoingData <- nearTowerCalls$outgoing[,c("ToTowerId", "Period")];
colnames(outgoingData) <- c("TowerId", "Period");
outgoingData <- merge( outgoingData, nearTowers, by="TowerId" )
outgoingData <- outgoingData[,c("TowerId", "Period", "Dist")];

PeriodCallsPerTower <- function( data, period, towerLocs ) {
  data <- data[data$Period==period,];
  calls <- data.frame(t(sapply( 1:nrow(towerLocs), function(i) {
    c(sum(data$TowerId == towerLocs$TowerId[i]), towerLocs$Dist[i]);
  } )));
  colnames(calls) <- c("Calls", "Dist");
  calls;
}

PlotPeriodCallsVsDistance <- function( data, period, towerLocs, callTitle ) {
  calls <- PeriodCallsPerTower( data, period, towerLocs );
#  calls$Calls <- calls$Calls / sum(calls$Calls);
  calls$Calls[is.nan(calls$Calls)] <- 0;
  periodTime <- paste(Pad2(floor(period/4)), ":", Pad2((period%%4)*15), sep="");
  plotTitle <- paste("Proportion of ", callTitle, " vs distance\n",
                     periodTime, sep="")
  ggplot(calls, aes(Dist, Calls)) +
    geom_point() +
    scale_x_log10() +
    labs(x="Distance (km)", y="Proportion of Calls", title=plotTitle);
}

setwd("C:/Plots/NetworkResearch/Event/TowerCallsVsDist");

interval <- 52:80;

pdf( "2011-07-23-IncomingCallsVsDist.pdf", height=7, width=10 );
for ( period in interval ) {
  print(PlotPeriodCallsVsDistance( incomingData, period, nearTowers, "incoming calls" ));
}
dev.off();

pdf( "2011-07-23-OutgoingCallsVsDist.pdf", height=7, width=10 );
for ( period in interval ) {
  print(PlotPeriodCallsVsDistance( outgoingData, period, nearTowers, "outgoing calls" ));
}
dev.off();

pdf( "2011-07-23-TotalCallsVsDist-2ndTower.pdf", height=7, width=10 );
for ( period in interval ) {
  print(PlotPeriodCallsVsDistance( rbind(incomingData, outgoingData),
                                   period, nearTowers, "total calls" ));
}
dev.off();


TowersByDist( 34.51828384, 69.18315125, towerLocs )[1:10,];


towerId <- 790;
day <- 6;
nMin <- 5;
eventHour <- 11;
eventMinute <- 52;

eventPeriod <- eventHour*60/nMin + floor(eventMinute/nMin);

towerInfo <- towers[towers$TowerId == towerId,];

nearTowers <- TowersByDist(towerInfo$Lat, towerInfo$Long, towers);


nearTowerCalls <- list(incoming=GetTowerInCalls( towerId, 11, 12 ),
                       outgoing=GetTowerOutCalls( towerId, 11, 12 ));

nearTowerCalls$incoming <- nearTowerCalls$incoming[nearTowerCalls$incoming$Day==day,];
nearTowerCalls$incoming$Period <- nearTowerCalls$incoming$Hour*60/nMin +
  floor(nearTowerCalls$incoming$Minute/nMin);
incomingData <- nearTowerCalls$incoming[,c("FromTowerId", "Period")];
colnames(incomingData) <- c("TowerId", "Period");
incomingData <- merge( incomingData, nearTowers, by="TowerId" )
incomingData <- incomingData[,c("TowerId", "Period", "Dist")];

nearTowerCalls$outgoing <- nearTowerCalls$outgoing[nearTowerCalls$outgoing$Day==day,];
nearTowerCalls$outgoing$Period <- nearTowerCalls$outgoing$Hour*60/nMin +
  floor(nearTowerCalls$outgoing$Minute/nMin);
outgoingData <- nearTowerCalls$outgoing[,c("ToTowerId", "Period")];
colnames(outgoingData) <- c("TowerId", "Period");
outgoingData <- merge( outgoingData, nearTowers, by="TowerId" )
outgoingData <- outgoingData[,c("TowerId", "Period", "Dist")];


periods <- (eventPeriod-40):(eventPeriod+40);

distVol <- data.frame(t(sapply(periods, function(period) {
  calls <- PeriodCallsPerTower( outgoingData, period, nearTowers );
###  calls <- calls[calls$Dist > 0,];
  c(mean(rep(calls$Dist, calls$Calls)), sd(rep(calls$Dist, calls$Calls)));
})));
colnames(distVol) <- c("meanDist", "sdDist");
distVol$period <- periods;

ggplot(distVol, aes(period, meanDist)) +
  geom_line()

setwd("C:/Plots/NetworkResearch/Event/TowerVolume/2011-12");

pdf("2011-12-Tower790-MonthVolume.pdf", height=12, width=12);
print(PlotMonthCallVolume( 790, 11, 12, interestingDay = 6 ));
dev.off();


interestingTowers <- c(790, 405, 636, 834, 183, 283, 840)

PlotMonthCallVolume( 790, 11, 12, interestingDay = 6, volCol="TotalDiffCalls" )

PlotMonthCallVolume( 405, 11, 12, interestingDay = 6 )
