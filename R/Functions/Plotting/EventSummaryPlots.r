require(ggplot2);
require(reshape2);

## Plots incoming and outgoing volume (difference from average)
## for the three days around an event at a given tower
Event3DayVolume <- function( year, month, day, hour,
                             minute, towerId, towerDist ) {
  
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
  data$DiffVol <- calls$InCalls - calls$OutCalls;

  threeDayTitle <- paste("Call volume difference for tower closest to event\n",
                         round(towerDist, 2), " km\nAverage volume: ",
                         round(avgCalls, 1), " calls per 5 min", sep="");


  ggplot(mdata, aes(x=Period, y=value, color=variable)) +
    geom_vline(xintercept=eventPeriod, color="darkgreen") +
    geom_vline(xintercept=nPeriods*(0:nDays), color="darkgray") +
    geom_line() +
    labs(x="Time", y="Volume - Ave(Volume)",
         title=threeDayTitle) +
    xTimeAxis(nPeriods, nDays) +
    scale_colour_manual(name="legend", guide="legend",
                        values=c("blue", "red"),
                        labels=c("Incoming", "Outgoing"));

}

## Generates a heatmap of calls into or out of a given tower
## based on either physical or geodesic distance
EventHeatmap <- function( year, month, day, hour,
                          minute, startHour, endHour,
                          towerCalls, nearTowers,
                          direction="Incoming",
                          distance="Physical",
                          radius=20 ) {

  eventTime <- 60*minute + 60*60*hour;

  callData <- NULL;
  callTable <- NULL;
  plotTitle <- "Incoming call volume to tower closest to event";
  yHelp <- "from calling tower";
  if ( direction == "Incoming" ) {
    callData <- towerCalls$incoming;
    callData$OtherTowerId <- callData$FromTowerId;
  } else {
    callData <- towerCalls$outgoing;
    callData$OtherTowerId <- callData$ToTowerId;
    plotTitle <- "Outgoing call volume from tower closest to event";
    yHelp <- "to called tower";
  }

  callData <- callData[callData$Day==day &
                       callData$Hour >= startHour &
                       callData$Hour <= endHour,];
  callData <- callData[,c("Hour", "Minute", "Second", "OtherTowerId")];
  colnames(callData) <- c("Hour", "Minute", "Second", "TowerId");
  callData <- callData[callData$TowerId != -1,];

  yLabel <- paste( "Distance", yHelp, "(km)" );
  if ( distance == "Physical" ) {
    callData <- merge( callData, nearTowers, by="TowerId" );
  } else {
    callTable <- table(callData$TowerId);
    geodesicDist <- data.frame(1/callTable);
    colnames(geodesicDist) <- c("TowerId", "Dist");
    callData <- merge( callData, geodesicDist, by="TowerId" );
    plotTitle <- paste(plotTitle, "Geodesic distance", sep="\n");
    yLabel <- paste( "Geodesic distance", yHelp, "(1/total volume)" );
  }
  callData <- callData[,c("Hour", "Minute", "Second", "Dist")];
  callData <- callData[callData$Dist <= radius,];


  callData$Time <- callData$Second + 60*callData$Minute +
    60*60*callData$Hour;
  callData$Time <- callData$Time - eventTime;
  callData$DiffMinute <- floor(callData$Time / 60);
  callData$Diff5Min <- floor(callData$Time / 300);
  callData$Dist <- callData$Dist + min( callData$Dist[callData$Dist > 0] ) / 10;

  xpts <- seq(from=-3*60, to=16*60, by=60);
  xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
             "+4 hr", "+5 hr", "+6 hr", "+7 hr", "+8 hr", "+9 hr",
             "+10 hr", "+11 hr", "+12 hr", "+13 hr", "+14 hr", "+15 hr", "+16 hr" );


  ggplot(callData, aes(x=DiffMinute, y=Dist)) +
    stat_density2d(aes(fill=..density..), geom="tile",
                   contour=FALSE, n=200) +
    scale_fill_gradient(low="black", high="red") +
    labs(x="Time From Event", y=yLabel, title=plotTitle) +
    scale_x_continuous( breaks=xpts, labels=xlabs ) +
    theme(legend.position="none");
}

EventCallVolume <- function(year, month, day, hour,
                            minute, startHour, endHour,
                            eventLat, eventLong, numTowers,
                            towerLocs, volCol="TotalStdCalls",
                            volName="Total standardized call volume") {

  startPeriod <- startHour*12;
  endPeriod <- endHour*12;
  eventPeriod <- hour*12 + minute/5;
  numTowers <- 60;

  towers <- TowersByDist( eventLat, eventLong, towerLocs );

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

  volData$Dist <- volData$Dist + min( volData$Dist[volData$Dist > 0] ) / 10;

  volData <- volData[,c("Period", "Dist", volCol)];
  colnames(volData)[3] <- "Volume";

  xpts <- seq(from=eventPeriod-36, to=eventPeriod+72, by=12);
  xlabs <- c("-3 hr", "-2 hr", "-1 hr", "Event", "+1 hr", "+2 hr", "+3 hr",
             "+4 hr", "+5 hr", "+6 hr");

  plotTitle <- paste(volName, "from towers near the event");

  ggplot(volData, aes(x=Period, y=Dist, z=Volume)) +
    stat_summary2d() +
    scale_fill_gradient(low="black", high="red") +
    labs(x="Time from event", y="Distance from tower to event (km)",
         title=plotTitle) +
    scale_x_continuous( breaks=xpts, labels=xlabs ) +
    theme(legend.position="none");
}
