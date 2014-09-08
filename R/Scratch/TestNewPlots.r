require(ggplot2);
require(reshape2);

setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");

towerLocs <- TowerLocations();

year <- 11;
month <- 12;
day <- 6;
hour <- 11;
minute <- 52;
second <- 0;
towerId <- 405;
eventTime <- second + 60*minute + 60*60*hour;

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
                             incomingData$Hour >= 10 &
                             incomingData$Hour <= 14,];
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

preData <- towerCalls$incoming;
preData <- preData[preData$Day==day &
                             preData$Hour >= 5 &
                             preData$Hour <= 9,];
preData <- preData[,c("Hour", "Minute", "Second", "FromTowerId")];
colnames(preData) <- c("Hour", "Minute", "Second", "TowerId");
preData <- merge( preData, nearTowers, by="TowerId" );
preData <- preData[,c("Hour", "Minute", "Second", "Dist")];
preData$Time <- preData$Second + 60*preData$Minute +
  60*60*preData$Hour;
preData$Time <- preData$Time - eventTime;
preData$DiffMinute <- floor(preData$Time / 60);
preData$Diff5Min <- floor(preData$Time / 300);
preData$Dist2 <- preData$Dist + 0.01;

xpts <- seq(from=-120, to=180, by=60);
xlabs <- c("-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr");

setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");

ggsave("Bombing-2011-12-06-DistVsGeoDist.pdf", height=10, width=10,
       plot=ggplot(incomingData, aes(Dist, GeoDist)) + geom_point());

ggsave("Bombing-2011-12-06-DistVsGeoDist100.pdf", height=10, width=10,
       plot=ggplot(incomingData[incomingData$Dist<100,], aes(Dist, GeoDist)) +
       geom_point());

ggsave("Bombing-2011-12-06-DistVsGeoDist25.pdf", height=10, width=10,
       plot=ggplot(incomingData[incomingData$Dist<25,], aes(Dist, GeoDist)) +
       geom_point());

ggsave("Bombing-2011-12-06-IncomingVolVsDist.pdf", height=10, width=10,
       plot=ggplot(incomingData[incomingData$Dist2 < 15,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance from calling tower (km)",
            title="Incoming call volume to tower closest to blast") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave("Bombing-2011-12-06-IncomingVolVsGeoDist.pdf", height=10, width=10,
       plot=ggplot(incomingData[incomingData$GeoDist < .003,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance from calling tower (1/total volume)",
            title="Incoming call volume to tower closest to blast\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));


## Outgoing

outgoingCallTable <- table(towerCalls$outgoing$ToTowerId);
geodesicDist <- data.frame(1/outgoingCallTable);
colnames(geodesicDist) <- c("TowerId", "GeoDist");

outgoingData <- towerCalls$outgoing;
outgoingData <- outgoingData[outgoingData$Day==day &
                             outgoingData$Hour >= 10 &
                             outgoingData$Hour <= 14,];
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

xpts <- seq(from=-120, to=180, by=60);
xlabs <- c("-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr");

setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");

ggsave("Bombing-2011-12-06-OutgoingVolVsDist.pdf", height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$Dist2 < 15,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Distance to called tower (km)",
            title="Outgoing call volume to tower closest to blast") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave("Bombing-2011-12-06-OutgoingVolVsGeoDist.pdf", height=10, width=10,
       plot=ggplot(outgoingData[outgoingData$GeoDist < .003,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time From Event", y="Geodesic distance to called tower (1/total volume)",
            title="Outgoing call volume to tower closest to blast\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none")); 





outgoingData <- towerCalls$outgoing;
outgoingData <- outgoingData[outgoingData$Day==day &
                             outgoingData$Hour >= 10 &
                             outgoingData$Hour <= 14,];
outgoingData <- outgoingData[,c("Hour", "Minute", "Second", "ToTowerId")];
colnames(outgoingData) <- c("Hour", "Minute", "Second", "TowerId");
outgoingData <- merge( outgoingData, nearTowers, by="TowerId" );
outgoingData <- outgoingData[,c("Hour", "Minute", "Second", "Dist")];
outgoingData$Time <- outgoingData$Second + 60*outgoingData$Minute +
  60*60*outgoingData$Hour;
outgoingData$Time <- outgoingData$Time - eventTime;
outgoingData$DiffMinute <- floor(outgoingData$Time / 60);

aggOut <- aggregate(outgoingData$Hour,
                    by=list(outgoingData$Dist, outgoingData$DiffMinute),
                    FUN=length)
colnames(aggOut) <- c("Dist", "DiffMinute", "Volume");

timeVolume <- aggregate(outgoingData$Hour,
                        by=list(outgoingData$DiffMinute),
                        FUN=length)
colnames(timeVolume) <- c("DiffMinute", "Volume");

indices <- 1:nrow(timeVolume);

totIndices <- sapply(aggOut$DiffMinute, function(m) {
  indices[timeVolume$DiffMinute==m]
})

aggOut$NormVol <- aggOut$Volume / timeVolume$Volume[totIndices];

outgoingData$Dist2 <- outgoingData$Dist + 0.01;

subdata <- outgoingData[outgoingData$DiffMinute >= -15 &
                        outgoingData$DiffMinute <= 30,]

ggplot(aggOut, aes(x=DiffMinute, y=Dist)) + 
  geom_bin2d(binwidth=c(1, 100)) +
  scale_y_log10();

ggplot(outgoingData, aes(x=DiffMinute, y=Dist2)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE) +
  scale_y_log10() +
  scale_fill_gradient(low="black", high="red");

ggplot(subdata, aes(x=DiffMinute, y=Dist2)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE) +
  scale_y_log10() +
  scale_fill_gradient(low="black", high="red")             

ggplot(outgoingData, aes(x=DiffMinute, y=Dist)) +
  stat_bin(aes(fill=..density..), geom="tile", position="identity") +
  scale_y_log10();

ggplot(outgoingData, aes(x=DiffMinute, y=Dist)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_y_log10();


ggplot(outgoingData, aes(x=DiffMinute, y=Dist)) +
  geom_point() +
  scale_y_log10();




##
             
setwd("C:/Data/NetworkResearch/PairedData/2011-12");

day6Data <- read.table("2011-12-06-Paired-Calls.txt", sep="|");
day6Data <- day6Data[,4:9];
colnames(day6Data) <- c("Hour", "Minute", "Second",
                        "FromTowerId", "ToTowerId", "Duration");

day6Data$Period5min <- day6Data$Hour * 12 + floor(day6Data$Minute / 5);

startPeriod <- 11*12;
endPeriod <- 14*12;

subdata.day6 <- day6Data[day6Data$Period5min >= startPeriod &
                         day6Data$Period5min <= endPeriod,]

subdata.day6$Time <- subdata.day6$Second + 60*subdata.day6$Minute +
  60*60*subdata.day6$Hour;
subdata.day6$Time <- subdata.day6$Time - eventTime;
subdata.day6$DiffMinute <- floor(subdata.day6$Time / 60);
subdata.day6$Diff5Min <- floor(subdata.day6$Time / 300);

outdata.day6 <- subdata.day6[,c("DiffMinute", "Diff5Min", "FromTowerId")];
colnames(outdata.day6) <- c("DiffMinute", "Diff5Min", "TowerId");
outdata.day6 <- merge( outdata.day6, nearTowers, by="TowerId" );
outdata.day6 <- merge( outdata.day6, geodesicDist, by="TowerId" );
outdata.day6$Dist2 <- outdata.day6$Dist + 0.01;
outdata.day6 <- outdata.day6[,c(2,3,7:9)];

ggplot(outdata.day6[outdata.day6$Dist < 15,], aes(x=DiffMinute, y=Dist)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE) +
  scale_fill_gradient(low="black", high="red")             


setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");

ggsave("Bombing-2011-12-06-TowerVolVsDist.pdf", height=10, width=10,
       plot=ggplot(outdata.day6[outdata.day6$Dist2 < 15,], aes(x=DiffMinute, y=Dist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time from event", y="Distance from tower to event (km)",
            title="Call volume from towers") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave("Bombing-2011-12-06-TowerVolVsGeoDist.pdf", height=10, width=10,
       plot=ggplot(outdata.day6[outdata.day6$GeoDist < .003,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time from event", y="Geodesic distance from tower (1/total volume)",
            title="Call volume from towers\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));







##

lat <- 34.51828384;
long <- 69.18315125;
year <- 11;
month <- 12;
day <- 6;
startPeriod <- 132;
endPeriod <- 168;
numTowers <- 60;

towers <- TowersByDist( lat, long, TowerLocations() );

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


ggplot(volData, aes(x=Period, y=as.factor(Dist))) + geom_tile(aes(fill=TotalStdCalls)) +
  scale_fill_gradient(low="black", high="red")             


towerAvgs <- aggregate(volData$TotalCalls, by=list(volData$TowerId), FUN=mean);
towerAvgs$x[towerAvgs$X==0] <- 1;
colnames(towerAvgs) <- c("TowerId", "AvgCalls");


indices <- 1:nrow(towerAvgs);

towerIndices <- sapply(volData$TowerId, function(tid) {
  indices[towerAvgs$TowerId==tid]
})

volData$TotalStdCalls2 <- volData$TotalDiffCall / towerAvgs$AvgCalls[towerIndices];

ggplot(volData, aes(x=Period, y=as.factor(Dist))) + geom_tile(aes(fill=TotalStdCalls2)) +
  scale_fill_gradient(low="black", high="red")             


ggplot(volData, aes(x=Period, y=Dist, z=TotalStdCalls)) + stat_summary2d();
ggplot(volData, aes(x=Period, y=Dist, z=TotalStdCalls2)) + stat_summary2d();

ggplot(volData, aes(x=Period, y=Dist, z=TotalStdCalls3)) + stat_summary2d();


setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");

xpts <- c(130, 142, 154, 166);
xlabs <- c("-1 hr", "Event", "+1 hr", "+2 hr");

ggsave("Bombing-2011-12-06-TowerStdVolVsDist.pdf", height=10, width=10,
       plot=ggplot(volData, aes(x=Period, y=Dist, z=TotalStdCalls3)) +
       stat_summary2d() +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time from event", y="Distance from tower to event (km)",
            title="Standardized call volume from towers near the event") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));

ggsave("Bombing-2011-12-06-TowerVolVsGeoDist.pdf", height=10, width=10,
       plot=ggplot(outdata.day6[outdata.day6$GeoDist < .003,],
         aes(x=DiffMinute, y=GeoDist)) +
       stat_density2d(aes(fill=..density..), geom="tile",
                      contour=FALSE, n=200) +
       scale_fill_gradient(low="black", high="red") +
       labs(x="Time from event", y="Geodesic distance from tower (1/total volume)",
            title="Call volume from towers\nGeodesic distance") +
       scale_x_continuous( breaks=xpts, labels=xlabs ) +
       theme(legend.position="none"));




## 3-day vol

towerId <- 790;
year <- 11;
month <- 12;
day <- 6;
hour <- 11;
minute <- 52;

calls <- GetTowerAggregatedCalls(towerId, year, month);
calls <- calls[calls$Day >= day-1 & calls$Day <= day+1,];
calls$Period <- 0:(nrow(calls)-1);

eventPeriod <- 288 + 12*hour + minute/5;
useEvent <- eventPeriod > 288;
eventTime <- "";

nPeriods=288;
nDays=3;

data <- data.frame(Period=0:(nPeriods*nDays-1),
                   InVol=calls$InDiffCalls,
                   OutVol=calls$OutDiffCalls);

mdata <- melt(data, id=c("Period"));



setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");

ggsave("Bombing-2011-12-06-ClosestTowerVolume.pdf", height=7, width=10,
       plot=ggplot(mdata, aes(x=Period, y=value, color=variable)) +
       geom_vline(xintercept=eventPeriod, color="darkgreen") +
       geom_vline(xintercept=nPeriods*(0:nDays), color="darkgray") +
       geom_line() +
       labs(x="Time", y="Volume - Ave(Volume)",
            title=paste("Call volume difference for tower closest to blast\n",
              "0.13 km\nAverage volume: 40 calls per 5 min", sep="")) +
       xTimeAxis(nPeriods, nDays) +
       scale_colour_manual(name="legend", guide="legend",
                           values=c("blue", "red"),
                           labels=c("Incoming", "Outgoing"))
);
