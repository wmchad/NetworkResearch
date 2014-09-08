## Script for plotting volume for the towers around a given location
## Arguments should be:
##   Year (YY)
##   Month (MM)
##   Day (dd)
##   Hour (hh)
##   Minute (mm)
##   Latitude
##   Longitude
##   Number of towers
##   Event Title
## Uses 5 minute aggregated data
## Example call: Rscript PlotVolumeVsDistance.r 11 12 06 11 52 34.51828384 69.18315125 40 Bombing

require(ggplot2);

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Functions");
source("Data/TowerFunctions.r");
source("Plotting/TowerVolumePlots.r");

PlotVolumeVsDistance <- function(year, month, day,
                                 startPeriod, endPeriod,
                                 lat, long,
                                 numTowers, eventTitle) {

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
    setwd(paste("/ischool/jb/roshan_anon/wmchad/Plots/Event/VolVsDist/20",
                year, "-", Pad2(month), sep=""));
    for ( vol in c("InCalls", "InDiffCalls", "InStdCalls", "OutCalls",
                   "OutDiffCalls", "OutStdCalls", "TotalCalls",
                   "TotalDiffCalls", "TotalStdCalls") ) {
        minCalls <- min(volData[,vol]);
        maxCalls <- max(volData[,vol]);
        plotFile <- paste("20", year, "-", Pad2(month), "-", Pad2(day),
                          "-", vol, "-VolVsDist.pdf", sep="")
        pdf(plotFile, height=7, width=15);
        for ( period in startPeriod:endPeriod ) {
            periodTime <- paste(Pad2(floor(period / 12)), ":",
                                Pad2(5 * period %% 12), sep="");
            plotTitle <- paste(eventTitle, ", 20", year, "-", Pad2(month),
                               "-", Pad2(day), " ", periodTime,
                               "\nEvent Time ", Pad2(hour), ":", Pad2(minute), sep="");
            periodData <- volData[volData$Period==period, c("Dist", vol)];
            colnames(periodData) <- c("Dist", "Volume");
            print(PlotVolVsDist(periodData, vol, plotTitle, minCalls, maxCalls));
        }
        dev.off();
    }
    
}
