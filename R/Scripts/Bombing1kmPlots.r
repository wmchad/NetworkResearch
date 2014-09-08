require(ggplot2);
require(reshape2);

setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");


towerLocs <- TowerLocations();

eventLat <- 34.51828384;
eventLong <- 69.18315125;

eventTowers <- TowersByDist(eventLat, eventLong, towerLocs);

year <- 11;
month <- 12;
day <- 6;
hour <- 11;
minute <- 52;
second <- 0;
startHour <- 10;
endHour <- 14;
radius <- 1;
eventTime <- second + 60*minute + 60*60*hour;
eventType <- "Bombing-1km";

plotRadius <- 15;
geodesicRadius <- .0005;

plotDir <- paste("C:/Plots/NetworkResearch/Event/Summaries/", eventType, "-20",
            year, "-", Pad2(month), "-", Pad2(day), sep="");
plotPrefix <- paste(eventType, "-20", year, "-", Pad2(month), "-", Pad2(day), sep="");

dir.create(plotDir, showWarnings=FALSE, recursive=TRUE);

#####################################
## month volume

setwd(plotDir);
pdf( paste(plotPrefix, "-NearestTowerMonthVolume.pdf", sep=""), height=10, width=10 );
print(PlotAggregateMonthCallVolume(eventLat, eventLong, 1, towerLocs,
                                   year, month, interestingDay = day,
                                   plotTitle="Aggregate tower call volume, 1 km"));
dev.off();


#####################################
## 3-day volume

calls <- GetRadiusAggregatedCalls(eventLat, eventLong, radius, towerLocs, year, month);
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

threeDayTitle <- paste("Call volume difference for towers within 1 km of event\n",
                       " km\nAverage volume: ",
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
## Incoming volume to nearby towers

towerCalls <- list(incoming=GetRadiusInCalls( eventLat, eventLong, radius,
                     towerLocs, year, month ),
                   outgoing=GetRadiusOutCalls( eventLat, eventLong, radius,
                     towerLocs, year, month ));


nearTowers <- TowersByDist(eventLat, eventLong,
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

xpts <- seq(from=-3*60, to=16*60, by=60);
xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
           "+4 hr", "+5 hr", "+6 hr", "+7 hr", "+8 hr", "+9 hr",
           "+10 hr", "+11 hr", "+12 hr", "+13 hr", "+14 hr", "+15 hr", "+16 hr" );

setwd(plotDir);

ggsave(paste(plotPrefix, "-IncomingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(incomingData[incomingData$Dist2 < plotRadius,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance from calling tower (km)",
            title="Incoming call volume to tower within 1km of event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave(paste(plotPrefix, "-IncomingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(incomingData[incomingData$GeoDist < geodesicRadius,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance from calling tower (1/total volume)",
            title="Incoming call volume to towers within 1km of event\nGeodesic distance") +
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

xpts <- seq(from=-3*60, to=16*60, by=60);
xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
           "+4 hr", "+5 hr", "+6 hr", "+7 hr", "+8 hr", "+9 hr",
           "+10 hr", "+11 hr", "+12 hr", "+13 hr", "+14 hr", "+15 hr", "+16 hr" );

setwd(plotDir);

ggsave(paste(plotPrefix, "-OutgoingVolVsDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$Dist2 < plotRadius,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance to called tower (km)",
            title="Outgoing call volume from towers within 1km of event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave(paste(plotPrefix, "-OutgoingVolVsGeoDist.pdf", sep=""), height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$GeoDist < geodesicRadius,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance to called tower (1/total volume)",
            title="Outgoing call volume from towers within 1km of event\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));



