require(ggplot2);
require(reshape2);

setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");


towerLocs <- TowerLocations();

eventLat <- 31.584;
eventLong <- 64.358;

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);

year <- 11;
month <- 11;
day <- 19;
hour <- 9;
minute <- 0;
second <- 0;
startHour <- 7;
endHour <- 15;
towerId <- eventTowers$TowerId[1];
towerDist <- eventTowers$Dist[1];
eventTime <- second + 60*minute + 60*60*hour;
eventType <- "SoccerMatch";
plotDir <- paste("C:/Plots/NetworkResearch/Event/Summaries/", eventType, "-20",
            year, "-", Pad2(month), "-", Pad2(day), sep="");
plotPrefix <- paste(eventType, "-20", year, "-", Pad2(month), "-", Pad2(day), sep="");

dir.create(plotDir, showWarnings=FALSE, recursive=TRUE);

#####################################
## month volume

setwd(plotDir);
pdf( paste(plotPrefix, "-NearestTowerMonthVolume.pdf", sep=""), height=10, width=10 );
print(PlotMonthCallVolume(towerId, year, month, interestingDay = day));
dev.off();


#####################################
## 3-day volume

calls <- GetTowerAggregatedCalls(towerId, year, month);
avgCalls <- sum(calls$TotalCalls) / (288*max(calls$Day));
calls <- calls[calls$Day >= day-1 & calls$Day <= day+1,];
calls$Period <- 0:(nrow(calls)-1);


eventPeriod <- 288 + 12*hour + minute/5;
useEvent <- eventPeriod > 288;

nPeriods=288;
nDays=3;

data <- data.frame(Period=0:(nPeriods*nDays-1),
                   InVol=calls$InDiffCalls,
                   OutVol=calls$OutDiffCalls);

mdata <- melt(data, id=c("Period"));

threeDayTitle <- paste("Call volume difference for tower closest to event\n",
                   round(towerDist, 2), " km\nAverage volume: ",
                   round(avgCalls, 1), " calls per 5 min", sep="");


setwd(plotDir);
ggsave( paste(plotPrefix, "-ClosestTowerVolume.pdf", sep=""), height=7, width=10,
       plot=ggplot(mdata, aes(x=Period, y=value, color=variable)) +
       geom_vline(xintercept=eventPeriod, color="darkgreen") +
       geom_vline(xintercept=nPeriods*(0:nDays), color="darkgray") +
       geom_line() +
       labs(x="Time", y="Volume - Ave(Volume)",
            title=threeDayTitle) +
       xTimeAxis(nPeriods, nDays) +
       scale_colour_manual(name="legend", guide="legend",
                           values=c("blue", "red"),
                           labels=c("Incoming", "Outgoing"))
);

#####################################
## Incoming volume to nearest tower

calls <- GetTowerAggregatedCalls(towerId, year, month);
towerCalls <- list(incoming=GetTowerInCalls( towerId, year, month ),
                       outgoing=GetTowerOutCalls( towerId, year, month ));

towerLat <- towerLocs$Lat[towerLocs$TowerId == towerId];
towerLong <- towerLocs$Long[towerLocs$TowerId == towerId];

nearTowers <- TowersByDist(towerLat, towerLong,
                           towerLocs);

incomingCallTable <- table(towerCalls$incoming$FromTowerId);
geodesicDist <- data.frame(1/incomingCallTable);
colnames(geodesicDist) <- c("TowerId", "GeoDist");

incomingData <- towerCalls$incoming;
incomingData <- incomingData[incomingData$Day==day &
                             incomingData$Hour >= startHour &
                             incomingData$Hour <= endHour,];
incomingData <- incomingData[,c("Hour", "Minute", "Second", "FromTowerId")];
colnames(incomingData) <- c("Hour", "Minute", "Second", "TowerId");
incomingData <- merge( incomingData, nearTowers, by="TowerId" );
incomingData <- merge( incomingData, geodesicDist, by="TowerId" );
incomingData <- incomingData[,c("Hour", "Minute", "Second", "Dist", "GeoDist")];
incomingData$Time <- incomingData$Second + 60*incomingData$Minute +
  60*60*incomingData$Hour;
incomingData$Time <- incomingData$Time - eventTime;
incomingData$DiffMinute <- floor(incomingData$Time / 60);
incomingData$Diff5Min <- floor(incomingData$Time / 300);
incomingData$Dist2 <- incomingData$Dist + 0.01;

xpts <- seq(from=-180, to=540, by=60);
xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
           "+4 hr", "+5 hr", "+6 hr", "+7 hr", "+8 hr", "+9 hr");

setwd(plotDir);

plotRadius <- 30;
geodesicRadius <- .0015;

ggsave(paste(plotPrefix, "-IncomingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(incomingData[incomingData$Dist2 < plotRadius,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance from calling tower (km)",
            title="Incoming call volume to tower closest to event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave(paste(plotPrefix, "-IncomingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(incomingData[incomingData$GeoDist < geodesicRadius,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance from calling tower (1/total volume)",
            title="Incoming call volume to tower closest to event\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));


##############################
## Outgoing call volume from nearest tower

outgoingCallTable <- table(towerCalls$outgoing$ToTowerId);
geodesicDist <- data.frame(1/outgoingCallTable);
colnames(geodesicDist) <- c("TowerId", "GeoDist");

outgoingData <- towerCalls$outgoing;
outgoingData <- outgoingData[outgoingData$Day==day &
                             outgoingData$Hour >= startHour &
                             outgoingData$Hour <= endHour,];
outgoingData <- outgoingData[,c("Hour", "Minute", "Second", "ToTowerId")];
colnames(outgoingData) <- c("Hour", "Minute", "Second", "TowerId");
outgoingData <- merge( outgoingData, nearTowers, by="TowerId" );
outgoingData <- merge( outgoingData, geodesicDist, by="TowerId" );
outgoingData <- outgoingData[,c("Hour", "Minute", "Second", "Dist", "GeoDist")];
outgoingData$Time <- outgoingData$Second + 60*outgoingData$Minute +
  60*60*outgoingData$Hour;
outgoingData$Time <- outgoingData$Time - eventTime;
outgoingData$DiffMinute <- floor(outgoingData$Time / 60);
outgoingData$Diff5Min <- floor(outgoingData$Time / 300);
outgoingData$Dist2 <- outgoingData$Dist + 0.01;

xpts <- seq(from=-180, to=540, by=60);
xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
           "+4 hr", "+5 hr", "+6 hr", "+7 hr", "+8 hr", "+9 hr");

setwd(plotDir);

plotRadius <- 30;
geodesicRadius <- .0015;

ggsave(paste(plotPrefix, "-OutgoingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$Dist2 < plotRadius,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance to called tower (km)",
            title="Outgoing call volume from tower closest to event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave(paste(plotPrefix, "-OutgoingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$GeoDist < geodesicRadius,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance to called tower (1/total volume)",
            title="Outgoing call volume from tower closest to event\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));



##############################
## Standardized call volume for towers near the event

startPeriod <- startHour*12;
endPeriod <- endHour*12;
eventPeriod <- hour*12 + minute/5;
numTowers <- 60;

towers <- TowersByDist( eventLat, eventLong, TowerLocations() );

volData <- data.frame(TowerId=0, Dist=0, Period=0,
                      InCalls=0, InAvgCalls=0,
                      OutCalls=0, OutAvgCalls=0,
                      TotalCalls=0, TotalAvgCalls=0);
dfIndex <- 1;
for ( i in 1:numTowers ) {
  calls <- GetTowerAggregatedCalls(towers$TowerId[i], year, month);
  if ( !is.null(calls) && nrow(calls) > 0 ) {
    calls <- calls[calls$Day == day &
                   calls$DayPeriod >= startPeriod &
                   calls$DayPeriod <= endPeriod,];
    volData[dfIndex:(dfIndex+nrow(calls)-1),] <- cbind(towers$TowerId[i],
                                                       towers$Dist[i],
                                                       calls[,c("DayPeriod",
                                                                "InCalls",
                                                                "InAvgCalls",
                                                                "OutCalls",
                                                                "OutAvgCalls",
                                                                "TotalCalls",
                                                                "TotalAvgCalls")]);
    dfIndex <- dfIndex+nrow(calls);
  }
}
volData$InDiffCalls <- volData$InCalls - volData$InAvgCalls;
volData$OutDiffCalls <- volData$OutCalls - volData$OutAvgCalls;
volData$TotalDiffCalls <- volData$TotalCalls - volData$TotalAvgCalls;
volData$InAvgCalls[volData$InAvgCalls < 1] <- 1;
volData$OutAvgCalls[volData$OutAvgCalls < 1] <- 1;
volData$TotalAvgCalls[volData$TotalAvgCalls < 1] <- 1;
volData$InStdCalls <- (volData$InCalls - volData$InAvgCalls) / sqrt(volData$InAvgCalls);
volData$OutStdCalls <- (volData$OutCalls - volData$OutAvgCalls) / sqrt(volData$OutAvgCalls);
volData$TotalStdCalls <- (volData$TotalCalls - volData$TotalAvgCalls) / sqrt(volData$TotalAvgCalls);
volData$TotalStdCalls3 <- (volData$TotalCalls - volData$TotalAvgCalls) / volData$TotalAvgCalls;

volData$Dist2 <- volData$Dist + 0.01;

towerAvgs <- aggregate(volData$TotalCalls, by=list(volData$TowerId), FUN=mean);
towerAvgs$x[towerAvgs$X==0] <- 1;
colnames(towerAvgs) <- c("TowerId", "AvgCalls");


indices <- 1:nrow(towerAvgs);

towerIndices <- sapply(volData$TowerId, function(tid) {
  indices[towerAvgs$TowerId==tid]
})

volData$TotalStdCalls2 <- volData$TotalDiffCall / towerAvgs$AvgCalls[towerIndices];

setwd(plotDir);

xpts <- seq(from=eventPeriod-36, to=eventPeriod+72, by=12);
xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
           "+4 hr", "+5 hr", "+6 hr");

ggsave(paste(plotPrefix, "-TowerStdVolVsDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(volData, aes(x=Period, y=Dist, z=TotalStdCalls3)) +
       stat_summary2d() +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time from event", y="Distance from tower to event (km)",
            title="Standardized call volume from towers near the event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

