setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Analysis/");
source("SwitchingAR.r");
setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Functions/Data/");
source("TowerFunctions.r");

GetTimeSeries <- function(data, day, interval) {
  y <- NULL;
  y$day <- data$TotalDiffCalls[data$Day == day];
  y$interval <- data$TotalDiffCalls[data$Day >= day-interval &
                                    data$Day <= day+interval];
  y$month <- data$TotalDiffCalls;
  y;
}

## Get data for the events

bombing1Calls <- GetTowerAggregatedCalls( 790, 11, 12 );
bombing2Calls <- GetTowerAggregatedCalls( 915, 11, 12 );
inaug1Calls <- GetTowerAggregatedCalls( 183, 11, 12 );
inaug2Calls <- GetTowerAggregatedCalls( 261, 11, 7 );

## Get various datasets:

bombing1.y <- GetTimeSeries( bombing1Calls, 6, 5 );
bombing2.y <- GetTimeSeries( bombing2Calls, 7, 5 );
inaug1.y <- GetTimeSeries( inaug1Calls, 15, 5 );
inaug2.y <- GetTimeSeries( inaug2Calls, 23, 5 );

events.y <- NULL;
events.y$day <- c(inaug1.y$day, bombing1.y$day, inaug2.y$day, bombing2.y$day);
events.y$day <- events.y$day/max(events.y$day);
events.y$interval <- c(inaug1.y$interval, bombing1.y$interval,
                       inaug2.y$interval, bombing2.y$interval);
events.y$interval <- events.y$interval/max(events.y$interval);
events.y$month <- c(inaug1.y$month, bombing1.y$month,
                       inaug2.y$month, bombing2.y$month);
events.y$month <- events.y$month/max(events.y$month);

## Analysis - concatenated events

verbose <- FALSE;
nAnnounce <- 100;
keepEvery <- 5;

setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Scripts");
tests <- read.table("ArHmmTestParams2.txt", header=TRUE);

for (i in 1:nrow(tests)) {
    nBurn <- tests$nBurn[i];
    nSamp <- tests$nSamp[1];
    K <- tests$K[i];
    hypers <- list(
        alpha = tests$alpha[i],
        a = tests$a[i],
        b=tests$b[i],
        sigma2mu=tests$sigma2mu[i],
        sigma2a=tests$sigma2a[i]);

    results <- SwitchingARSampler(hypers, K, events.y$day,
                                  nBurn, nSamp, verbose=verbose,
                                  nAnnounce=nAnnounce,
                                  keepEvery=keepEvery);
    setwd("/ischool/jb/roshan_anon/wmchad/Code/R/Scripts/ArHmmResults");
    save(results, file=paste(tests$Name[i], ".results.rdata", sep=""));
}
