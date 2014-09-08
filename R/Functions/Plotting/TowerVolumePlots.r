require(lubridate);

## Builds an x-axis for a given number of days with the proper time labels
xTimeAxis <- function( nPeriods=288, nDays=1 ) {
    xpts <- nPeriods / 4 * 0:(4*nDays);
    xlabs <- c(rep(c( "12:00 AM", "6:00 AM", "12:00 PM", "6:00 PM" ), nDays), "12:00 AM" );
    scale_x_continuous( breaks=xpts, labels=xlabs );
}

xTimeAxisShort <- function( nPeriods=288, nDays=1 ) {
    xpts <- nPeriods / 2 * 0:(2*nDays);
    xlabs <- c(rep(c( "12AM", "12PM" ), nDays), "12AM" );
    scale_x_continuous( breaks=xpts, labels=xlabs );
}

## Plots the call volume given over a number of days
## Can add a vertical line to mark an event
PlotCallVolume <- function( callVol, nPeriods=288, nDays=1,
                            yLabel="Volume", plotTitle="",
                            addEvent=FALSE, eventPeriod=0 ) {

    data <- data.frame(Period=0:(nPeriods*nDays-1), Volume=callVol);
    if (addEvent) {
        ggplot(data, aes(x=Period, y=Volume)) +
            geom_vline(xintercept=eventPeriod, color="red") +
            geom_line() + labs(x="Time", y=yLabel, title=plotTitle) +
            xTimeAxis(nPeriods, nDays);
    } else {
        ggplot(data, aes(x=Period, y=Volume)) +
            geom_line() + labs(x="Time", y=yLabel, title=plotTitle) +
            xTimeAxis(nPeriods, nDays);
    }
}

## Plots the call volume for a set of days
PlotMultipleDayVolume <- function( callVol, year, month, startDay, endDay,
                                   nPeriods=288, addWeekDays=FALSE, yLabel="Volume",
                                   plotTitle=NULL, addEvent=FALSE, eventPeriod=0,
                                   interestingDay=-1, yrng=NULL) {
  nDays <- endDay - startDay + 1;
  data <- data.frame(Period=0:(length(callVol)-1), Volume=callVol);
  days <- Date.ymd(year, month, max(1,startDay):endDay);
  wdays <- weekdays(days, T);
  dayTitles <- paste(wdays, day(days), sep="\n");
  dayTitles <- day(days);
  if ( startDay < 1 ) {
    dayTitles <- c(rep("", 1-startDay), dayTitles);
  }
  volPlot <- ggplot(data, aes(x=Period, y=Volume)) +
    geom_vline(xintercept=nPeriods*(0:nDays), color="darkgray");
  if (addEvent) {
    volPlot <- volPlot + geom_vline(xintercept=eventPeriod, color="red")
  }
  ymax <- .9*max(callVol);
  if ( !is.null(yrng) ) {
    volPlot <- volPlot + ylim(yrng);
    ymax <- .9*yrng[2];
  }
  volPlot <- volPlot +geom_line() + labs(x="Time", y=yLabel, title=plotTitle) +
    xTimeAxisShort(nPeriods, nDays) +
    annotate("text", x=nPeriods*(0:(nDays-1))+(nPeriods/5), y=ymax,
             label=dayTitles, color="Blue");
  if (startDay <= interestingDay && interestingDay <= endDay) {
    volPlot <- volPlot + annotate("text", x=nPeriods*(interestingDay-startDay)+(nPeriods/5),
                                  y=ymax,
                                  label=dayTitles[1+interestingDay-startDay],
                                  color="Red");
  }
  volPlot;
}

## Plots volume versus distance
PlotVolVsDist <- function(data, yLabel, plotTitle, minCalls, maxCalls) {
    ggplot(data, aes(x=Dist, y=Volume)) + geom_point() +
        stat_smooth(method="loess") +
        labs(x="Distance (km)", y=yLabel, title=plotTitle) +
        ylim(minCalls, maxCalls);
}

## Plots volume for a tower for a full month
PlotMonthCallVolume <- function( towerId, year, month,
                                 volCol="TotalCalls",
                                 interestingDay=-1,
                                 plotTitle="Nearest Tower Call Volume" ) {
  fullYear <- 2000 + year;
  calls <- GetTowerAggregatedCalls(towerId, year, month);
  callVol <- calls[,volCol];
  yrng <- c(min(callVol), max(callVol));

  nextYear <- fullYear;
  nextMonth <- month+1;
  if ( nextMonth > 12 ) {
    nextYear <- nextYear + 1;
    nextMonth <- 1;
  }
  nDays <- as.numeric(difftime(Date.ymd(nextYear, nextMonth, 1), Date.ymd(fullYear, month, 1)));
  wd <- weekdays(Date.ymd(fullYear, month, 1:7), TRUE);
  firstDay <- (1:7)[wd == "Sun"];
  if ( firstDay > 1 ) { firstDay <- firstDay - 7; }
  nWeeks <- ceiling((nDays - firstDay + 1)/7);
  startDays <- firstDay +(0:nWeeks)*7;
  endDays <- startDays + 6;
  w1data <- c(rep(0, 288*(1-firstDay)), callVol[calls$Day>=startDays[1] &
                                              calls$Day<=endDays[1]]);
  p1 <- PlotMultipleDayVolume(w1data, fullYear, month, startDays[1], endDays[1],
                              plotTitle=paste(plotTitle, ", ",
                                fullYear, "-", Pad2(month), sep=""),
                              yrng=yrng, interestingDay=interestingDay);
  w2data <- callVol[calls$Day>=startDays[2] & calls$Day<=endDays[2]];
  p2 <- PlotMultipleDayVolume(w2data, fullYear, month, startDays[2], endDays[2],
                              yrng=yrng, interestingDay=interestingDay);
  w3data <- callVol[calls$Day>=startDays[3] & calls$Day<=endDays[3]];
  p3 <- PlotMultipleDayVolume(w3data, fullYear, month, startDays[3], endDays[3],
                              yrng=yrng, interestingDay=interestingDay);
  w4data <- callVol[calls$Day>=startDays[4] & calls$Day<=endDays[4]];
  p4 <- PlotMultipleDayVolume(w4data, fullYear, month, startDays[4], endDays[4],
                              yrng=yrng, interestingDay=interestingDay);
  if ( nWeeks > 4 ) {
    w5data <- callVol[calls$Day>=startDays[5] & calls$Day<=endDays[5]];
    if ( endDays[5] > nDays ) {
      w5data <- c(w5data, rep(0, 288*(endDays[5]-nDays-1)));
    }
    p5 <- PlotMultipleDayVolume(w5data, fullYear, month, startDays[5], endDays[5],
                                yrng=yrng, interestingDay=interestingDay);
    if ( nWeeks > 5 ) {
      w6data <- callVol[calls$Day>=startDays[6] & calls$Day<=endDays[6]];
      if ( endDays[6] > nDays ) {
        w5data <- c(w6data, rep(0, 288*(endDays[6]-nDays-1)));
      }
      p6 <- PlotMultipleDayVolume(w6data, fullYear, month, startDays[6], endDays[6],
                                  yrng=yrng, interestingDay=interestingDay);
      multiplot(p1,p2,p3,p4,p5,p6);
    }
    else { multiplot(p1,p2,p3,p4,p5); }
  }
  else { multiplot(p1,p2,p3,p4); }
}

## Plots volume for all towers within a radius of a location for a full month
PlotAggregateMonthCallVolume <- function( lat, long, radius, towerLocs,
                                         year, month,
                                         volCol="TotalCalls",
                                         interestingDay=-1,
                                         plotTitle="Nearest Tower Call Volume" ) {
  fullYear <- 2000 + year;
  calls <- GetRadiusAggregatedCalls(lat, long, radius, towerLocs, year, month);
  callVol <- calls[,volCol];
  yrng <- c(min(callVol), max(callVol));

  nextYear <- fullYear;
  nextMonth <- month+1;
  if ( nextMonth > 12 ) {
    nextYear <- nextYear + 1;
    nextMonth <- 1;
  }
  nDays <- as.numeric(difftime(Date.ymd(nextYear, nextMonth, 1), Date.ymd(fullYear, month, 1)));
  wd <- weekdays(Date.ymd(fullYear, month, 1:7), TRUE);
  firstDay <- (1:7)[wd == "Sun"];
  if ( firstDay > 1 ) { firstDay <- firstDay - 7; }
  nWeeks <- ceiling((nDays - firstDay + 1)/7);
  startDays <- firstDay +(0:nWeeks)*7;
  endDays <- startDays + 6;
  w1data <- c(rep(0, 288*(1-firstDay)), callVol[calls$Day>=startDays[1] &
                                              calls$Day<=endDays[1]]);
  p1 <- PlotMultipleDayVolume(w1data, fullYear, month, startDays[1], endDays[1],
                              plotTitle=paste(plotTitle, ", ",
                                fullYear, "-", Pad2(month), sep=""),
                              yrng=yrng, interestingDay=interestingDay);
  w2data <- callVol[calls$Day>=startDays[2] & calls$Day<=endDays[2]];
  p2 <- PlotMultipleDayVolume(w2data, fullYear, month, startDays[2], endDays[2],
                              yrng=yrng, interestingDay=interestingDay);
  w3data <- callVol[calls$Day>=startDays[3] & calls$Day<=endDays[3]];
  p3 <- PlotMultipleDayVolume(w3data, fullYear, month, startDays[3], endDays[3],
                              yrng=yrng, interestingDay=interestingDay);
  w4data <- callVol[calls$Day>=startDays[4] & calls$Day<=endDays[4]];
  p4 <- PlotMultipleDayVolume(w4data, fullYear, month, startDays[4], endDays[4],
                              yrng=yrng, interestingDay=interestingDay);
  if ( nWeeks > 4 ) {
    w5data <- callVol[calls$Day>=startDays[5] & calls$Day<=endDays[5]];
    if ( endDays[5] > nDays ) {
      w5data <- c(w5data, rep(0, 288*(endDays[5]-nDays-1)));
    }
    p5 <- PlotMultipleDayVolume(w5data, fullYear, month, startDays[5], endDays[5],
                                yrng=yrng, interestingDay=interestingDay);
    if ( nWeeks > 5 ) {
      w6data <- callVol[calls$Day>=startDays[6] & calls$Day<=endDays[6]];
      if ( endDays[6] > nDays ) {
        w5data <- c(w6data, rep(0, 288*(endDays[6]-nDays-1)));
      }
      p6 <- PlotMultipleDayVolume(w6data, fullYear, month, startDays[6], endDays[6],
                                  yrng=yrng, interestingDay=interestingDay);
      multiplot(p1,p2,p3,p4,p5,p6);
    }
    else { multiplot(p1,p2,p3,p4,p5); }
  }
  else { multiplot(p1,p2,p3,p4); }
}
