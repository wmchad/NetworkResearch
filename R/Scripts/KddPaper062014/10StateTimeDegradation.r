require(ggplot2);
setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");

GetTimeSeries <- function(data, day, interval) {
  y <- NULL;
  y$day <- data$TotalDiffCalls[data$Day == day];
  y$interval <- data$TotalDiffCalls[data$Day >= day-interval &
                                    data$Day <= day+interval];
  y$month <- data$TotalDiffCalls;
  y;
}

GetSamplerTransitions <- function( sampler ) {
  counts <- rep(0, length(sampler$samples[[1]]$z)-1);
  for ( sample in sampler$samples ) {
    counts <- counts + as.numeric(sample$z[-1]!=sample$z[-length(sample$z)]);
  }
  c(0, counts);
}

GetPerformance <- function( probs, nDays, inaugPeriods, bombPeriods,
                            inaug1Event, bomb1Event, inaug2Event, bomb2Event) {
  inaug1 <- probs[1:(nDays*288)];
  bomb1 <- probs[(nDays*288+1):(2*nDays*288)];
  inaug2 <- probs[(2*nDays*288+1):(3*nDays*288)];
  bomb2 <- probs[(3*nDays*288+1):(4*nDays*288)];

  inaug1 <- GetWindowProbs(inaug1, inaugPeriods, inaug1Event);
  bomb1 <- GetWindowProbs(bomb1, bombPeriods, bomb1Event);
  inaug2 <- GetWindowProbs(inaug2, inaugPeriods, inaug2Event);
  bomb2 <- GetWindowProbs(bomb2, bombPeriods, bomb2Event);

  fullProbs <- c(inaug1$probs, bomb1$probs, inaug2$probs, bomb2$probs);
  eventProbs <- c(inaug1$evProb, bomb1$evProb, inaug2$evProb, bomb2$evProb);
  nFalse <- length(fullProbs) - 4;

  inaug1FDR <- (sum(fullProbs >= inaug1$evProb) - sum(eventProbs >= inaug1$evProb)) / nFalse;
  bomb1FDR <- (sum(fullProbs >= bomb1$evProb) - sum(eventProbs >= bomb1$evProb)) / nFalse;
  inaug2FDR <- (sum(fullProbs >= inaug2$evProb) - sum(eventProbs >= inaug2$evProb)) / nFalse;
  bomb2FDR <- (sum(fullProbs >= bomb2$evProb) - sum(eventProbs >= bomb2$evProb)) / nFalse;

  data.frame( Event=c("inaug1", "bomb1", "inaug2", "bomb2"),
              ProbDet=c(inaug1$evProb, bomb1$evProb, inaug2$evProb, bomb2$evProb),
              FDR=c(inaug1FDR, bomb1FDR, inaug2FDR, bomb2FDR) );
}

GetWindowMaxes <- function( data, wSize, start ) {
  N <- length(data);
  nWindows <- floor((N-start) / wSize);
  sapply( 0:(nWindows-1), function(w) {
    max(data[(w*wSize)+start+(0:(wSize-1))]);
  } );
}

GetWindowProbs <- function( probs, periods, event ) {
  wSize <- periods * 2 + 1;
  startIndex <- event - 1 - periods;
  eventWindow <- 1;
  while ( startIndex > wSize ) {
    startIndex <- startIndex - wSize;
    eventWindow <- eventWindow + 1;
  }
  probMaxes <- GetWindowMaxes(probs, wSize, startIndex);
  list(probs=probMaxes, evProb=probMaxes[eventWindow]);
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


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("arhmmResults.events.day.10s.rdata");

changeProb.day <- GetSamplerTransitions(arhmmResults.events.day.10s) /
  length(arhmmResults.events.day.10s$samples);
nDays <- 1;
inaugPeriods <- 3;
bombPeriods <- 1;
inaug1Event <- 93;
bomb1Event <- 142;
inaug2Event <- 169;
bomb2Event <- 135;

GetPerformance(changeProb.day, nDays, inaugPeriods, bombPeriods,
               inaug1Event, bomb1Event, inaug2Event, bomb2Event);



load("arhmmResults.events.week.10s.rdata");

changeProb.week <- GetSamplerTransitions(arhmmResults.events.week.10s) /
  length(arhmmResults.events.week.10s$samples);

nDays <- 7;
inaugPeriods <- 3;
bombPeriods <- 1;
inaug1Event <- 3*288+93;
bomb1Event <- 3*288+142;
inaug2Event <- 3*288+169;
bomb2Event <- 3*288+135;

GetPerformance(changeProb.week, nDays, inaugPeriods, bombPeriods,
               inaug1Event, bomb1Event, inaug2Event, bomb2Event);

load("arhmmResults.events.month.10s.rdata");

changeProb.month <- GetSamplerTransitions(arhmmResults.events.month.10s) /
  length(arhmmResults.events.month.10s$samples);

nDays <- 31;
inaugPeriods <- 3;
bombPeriods <- 1;
inaug1Event <- 14*288+93;
bomb1Event <- 5*288+142;
inaug2Event <- 22*288+169;
bomb2Event <- 6*288+135;

GetPerformance(changeProb.month, nDays, inaugPeriods, bombPeriods,
               inaug1Event, bomb1Event, inaug2Event, bomb2Event);


plot(events.y$month[10351:10650], type="l");
abline(v=160, col="red");
lines(changeProb.month[10351:10650], col="green");


plot(events.y$month, type="l")
abline(v=31*288, col="red")
abline(v=62*288, col="red")
abline(v=93*288, col="red")
abline(v=14*288+93, col="blue")
abline(v=36*288+142, col="blue")
abline(v=88*288+169, col="blue")
abline(v=99*288+135, col="blue")
lines(changeProb.month, col="green")

plot(events.y$month[10501:10520], type="l")
lines(changeProb.month[10501:10520], col="red")
