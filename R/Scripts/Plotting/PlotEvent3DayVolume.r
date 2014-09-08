## Script for plotting volume for a tower for the 3 days around a given day
## Arguments should be:
##   Tower Id
##   Year (YY)
##   Month (MM)
##   Day (dd)
##   Hour (hh)
##   Minute (mm)
## Uses 5 minute aggregated data
## Example call: Rscript PlotEvent3DayVolume.r 405 11 12 06 11 52 Bombing 0.249

require(ggplot2);

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");

PlotEvent3DayVolume <- function( towerId, year, month, day,
                                 hour, minute, eventTitle, dist ) {
    ## Get the aggregated call files
    ## columns: Day, DayPeriod, Period, InCalls, InAvgCalls,
    ##          OutCalls, OutAvgCalls, TotalCalls, TotalAvgCalls
    calls <- GetTowerAggregatedCalls(towerId, year, month);

    if ( !is.null(calls) && nrow(calls) > 0 ) {
        ## Only get the three days we're interested in
        calls <- calls[calls$Day >= day-1 & calls$Day <= day+1,];
        calls$Period <- 0:(nrow(calls)-1);

        eventPeriod <- 288 + 12*hour + minute/5;
        useEvent <- eventPeriod > 288;
        eventTime <- "";
        if ( useEvent ) { eventTime <- paste(", ", hour, ":", Pad2(minute), sep=""); }

        PlotTitle <- function( avgVol ) {
            paste(eventTitle, ", 20", year, "-", Pad2(month), "-", Pad2(day), eventTime,
                  "\nTower Distance: ", dist, " km",
                  "\nAverage Volume: ", avgVol,
                  " calls per 5 min", sep="");
        }

        print(paste("Setting directory to /ischool/jb/roshan_anon/wmchad/Plots/Event/TowerVolume/20",
                    year, "-", Pad2(month), sep=""));
        setwd(paste("/ischool/jb/roshan_anon/wmchad/Plots/Event/TowerVolume/20",
                    year, "-", Pad2(month), sep=""));

        pdf(paste("20", year, "-", Pad2(month), "-", Pad2(day), "-Tower", towerId, "-3dayVolume.pdf", sep=""),
            height=7, width=15);
        print(PlotCallVolume( calls$InCalls, nPeriods=288, nDays=3,
                      yLabel="Incoming Total Volume",
                      plotTitle=PlotTitle(round(mean(calls$InCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        print(PlotCallVolume( calls$InCalls - calls$InAvgCalls, nPeriods=288, nDays=3,
                      yLabel="Incoming Difference Volume",
                      plotTitle=PlotTitle(round(mean(calls$InCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        print(PlotCallVolume( calls$OutCalls, nPeriods=288, nDays=3,
                      yLabel="Outgoing Total Volume",
                      plotTitle=PlotTitle(round(mean(calls$OutCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        print(PlotCallVolume( calls$OutCalls - calls$OutAvgCalls, nPeriods=288, nDays=3,
                      yLabel="Outgoing Difference Volume",
                      plotTitle=PlotTitle(round(mean(calls$OutCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        print(PlotCallVolume( calls$TotalCalls, nPeriods=288, nDays=3,
                      yLabel="Total Volume",
                      plotTitle=PlotTitle(round(mean(calls$TotalCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        print(PlotCallVolume( calls$TotalCalls - calls$TotalAvgCalls, nPeriods=288, nDays=3,
                      yLabel="Total Difference Volume",
                      plotTitle=PlotTitle(round(mean(calls$TotalCalls), 1)),
                      addEvent=useEvent, eventPeriod=eventPeriod ));
        dev.off();
    }
}


