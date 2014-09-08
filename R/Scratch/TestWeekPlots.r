setwd("C:/Code/NetworkResearch/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");
source("Plotting/PlotUtilities.r");

year <- 11;
month <- 12;
towerId <- 405;
calls <- GetTowerAggregatedCalls(towerId, year, month);

subdata <- calls[calls$Day >= 1 & calls$Day <= 7,];

PlotMultipleDayVolume( subdata$TotalCalls, 2011, 12, 1, 7, plotTitle="My Title" );
PlotMultipleDayVolume( subdata$TotalCalls - subdata$TotalAvgCalls,
                       2011, 12, 1, 7, plotTitle="My Title" );
PlotMultipleDayVolume( (subdata$TotalCalls - subdata$TotalAvgCalls) / max(1, subdata$TotalAvgCalls),
                       2011, 12, 1, 7, plotTitle="My Title" );

subdata2 <- calls[calls$Day >= 8 & calls$Day <= 14,];
PlotMultipleDayVolume( subdata2$TotalCalls, 2011, 12, 8, 14, plotTitle="My Title" );
PlotMultipleDayVolume( subdata2$TotalCalls - subdata$TotalCalls,
                       2011, 12, 8, 14, plotTitle="My Title" );

subdata3 <- calls[calls$Day >= 15 & calls$Day <= 21,];
PlotMultipleDayVolume( subdata3$TotalCalls - subdata2$TotalCalls,
                       2011, 12, 15, 21, plotTitle="My Title" );
PlotMultipleDayVolume( subdata3$TotalCalls - subdata3$TotalAvgCalls,
                       2011, 12, 15, 21, plotTitle="My Title" );
PlotMultipleDayVolume( subdata3$TotalCalls,
                       2011, 12, 15, 21, plotTitle="My Title" );

firstweekdata <- c(rep(0, 288*4), calls[calls$Day >=1 & calls$Day <= 3,]$TotalCalls);
secondweekdata <- calls[calls$Day >=4 & calls$Day <= 10,]$TotalCalls;
thirdweekdata <- calls[calls$Day >=11 & calls$Day <= 17,]$TotalCalls;
fourthweekdata <- calls[calls$Day >=18 & calls$Day <= 24,]$TotalCalls;
fifthweekdata <- calls[calls$Day >=25 & calls$Day <= 31,]$TotalCalls;

wk1Plot <- PlotMultipleDayVolume( firstweekdata, 2011, 12, -3, 3,
                                  plotTitle="Month Call Volume, December 2011" );
wk2Plot <- PlotMultipleDayVolume( secondweekdata, 2011, 12, 4, 10 );
wk3Plot <- PlotMultipleDayVolume( thirdweekdata, 2011, 12, 11, 17 );
wk4Plot <- PlotMultipleDayVolume( fourthweekdata, 2011, 12, 18, 24 );
wk5Plot <- PlotMultipleDayVolume( fifthweekdata, 2011, 12, 25, 31 );

multiplot( wk1Plot, wk2Plot, wk3Plot, wk4Plot, wk5Plot );

PlotMonthCallVolume <- function( towerId, year, month,
                                 volCol="TotalCalls",
                                 interestingDay=-1) {
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
                              plotTitle=paste("Nearest Tower Call Volume, ",
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


setwd("C:/Plots/NetworkResearch/Event/Summaries/Bombing-2011-12-06");
pdf( "Bombing-2011-12-06-NearestTowerMonthVolume.pdf", height=10, width=10 );
print(PlotMonthCallVolume( 790, 11, 12, interestingDay = 6 ));
dev.off();


PlotMonthCallVolume( 790, 11, 11, interestingDay = 7 )
