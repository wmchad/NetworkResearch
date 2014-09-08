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

load("results.bombing.day.3s.rdata");

bestSample.bombing.day.3s <- samplerResults.bombing.day.3s$bestSample;

tempz <- bestSample.bombing.day.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.day.3s <- data.frame(time=1:length(bombing1.y$day),
                                volume=bombing1.y$day / max(bombing1.y$day),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.1day.3s.png", height=7, width=14,
        plot=ggplot(df.bombing.day.3s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("results.bombing.interval.3s.rdata");

bestSample.bombing.interval.3s <- samplerResults.bombing.interval.3s$bestSample;

tempz <- bestSample.bombing.interval.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.interval.3s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.11day.3s.png", height=7, width=14,
        plot=ggplot(df.bombing.interval.3s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("results.bombing.interval.5s.rdata");

bestSample.bombing.interval.5s <- samplerResults.bombing.interval.5s$bestSample;

tempz <- bestSample.bombing.interval.5s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.interval.5s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.11day.5s.png", height=7, width=14,
        plot=ggplot(df.bombing.interval.5s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("results.bombing.interval.10s.rdata");

bestSample.bombing.interval.10s <- samplerResults.bombing.interval.10s$bestSample;

tempz <- bestSample.bombing.interval.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.interval.10s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.11day.10s.png", height=7, width=14,
        plot=ggplot(df.bombing.interval.10s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.3s.rdata");

bestSample.events.day.3s <- samplerResults.events.day.3s$bestSample;

tempz <- bestSample.events.day.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.events.day.3s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day,
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.1day.3s.png", height=7, width=14,
        plot=ggplot(df.events.day.3s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


## Decent results

load("results.events.day.5s.rdata");

bestSample.events.day.5s <- samplerResults.events.day.5s$bestSample;

bs <- bestSample.events.day.5s;

tempz <- bestSample.events.day.5s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.events.day.5s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day,
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.1day.5s.png", height=7, width=14,
        plot=ggplot(df.events.day.5s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.interval.5s.rdata");

bestSample.events.interval.5s <- samplerResults.events.interval.5s$bestSample;

tempz <- bestSample.events.interval.5s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.events.interval.5s <- data.frame(time=1:length(events.y$interval),
                                volume=events.y$interval,
                                z=bestSample.events.interval.5s$z);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.interval.5s.png", height=7, width=14,
        plot=ggplot(df.events.interval.5s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


load("results.events.interval.3s.rdata");

bestSample.events.interval.3s <- samplerResults.events.interval.3s$bestSample;

tempz <- bestSample.events.interval.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.events.interval.3s <- data.frame(time=1:length(events.y$interval),
                                volume=events.y$interval,
                                z=tempz);

ggplot(df.events.interval.3s, aes(x=time)) +
  geom_line(aes(y=volume), color="blue") +
  geom_step(aes(y=z), color="red") +
  labs(x="Time", y="Volume")


## Decent result

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.10s.rdata");

bestSample.events.day.10s <- samplerResults.events.day.10s$bestSample;

tempz <- bestSample.events.day.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;

df.events.day.10s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day,
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.1day.10s.png", height=7, width=14,
        plot=ggplot(df.events.day.10s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );



setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.interval.10s.rdata");

bestSample.events.interval.10s <- samplerResults.events.interval.10s$bestSample;

tempz <- bestSample.events.interval.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;

df.events.interval.10s <- data.frame(time=1:length(events.y$interval),
                                volume=events.y$interval,
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.11day.10s.png", height=7, width=14,
        plot=ggplot(df.events.interval.10s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");




setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("results.events.day.3s.long.rdata");

bestSample.events.day.long.3s <- samplerResults.events.day.long.3s$bestSample;

tempz <- bestSample.events.day.long.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

counts <- rep(0, length(samplerResults.events.day.long.3s$samples[[1]]$z)-1);
for ( sample in samplerResults.events.day.long.3s$samples ) {
  counts <- counts + as.numeric(sample$z[-1]!=sample$z[-length(sample$z)]);
}
counts <- c(0, counts);
counts <- counts / 2000;

df.events.day.long.3s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day / max(events.y$day),
                                z=tempz/3-1, prob=counts/2 - 1.5);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

setwd("C:/Data/NetworkResearch/Results");
ggsave( "AR-HMM-3State.png", height=5, width=10,
        plot=ggplot(df.events.day.long.3s, aes(x=time)) +
        geom_line(aes(y=volume)) +
        geom_step(aes(y=z), color="blue") +
        geom_line(aes(y=prob), color="darkgreen") +
        labs(x="Time", y="Volume") );



setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.5s.long.rdata");

bestSample.events.day.long.5s <- samplerResults.events.day.long.5s$bestSample;

tempz <- bestSample.events.day.long.5s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

counts <- rep(0, length(samplerResults.events.day.long.5s$samples[[1]]$z)-1);
for ( sample in samplerResults.events.day.long.5s$samples ) {
  counts <- counts + as.numeric(sample$z[-1]!=sample$z[-length(sample$z)]);
}
counts <- c(0, counts);
counts <- counts / 2000;

df.events.day.long.5s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day / max(events.y$day),
                                z=(tempz/5)-1, prob=counts/2 - 1.5);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

setwd("C:/Data/NetworkResearch/Results");
ggsave( "AR-HMM-5State.png", height=6, width=10,
        plot=ggplot(df.events.day.long.5s, aes(x=time)) +
        geom_line(aes(y=volume)) +
        geom_step(aes(y=z), color="blue") +
        geom_line(aes(y=prob), color="darkgreen") +
        labs(x="Time", y="Volume") );


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.10s.long.rdata");

bestSample.events.day.long.10s <- samplerResults.events.day.long.10s$bestSample;

tempz <- bestSample.events.day.long.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;

counts <- rep(0, length(samplerResults.events.day.long.10s$samples[[1]]$z)-1);
for ( sample in samplerResults.events.day.long.10s$samples ) {
  counts <- counts + as.numeric(sample$z[-1]!=sample$z[-length(sample$z)]);
}
counts <- c(0, counts);
counts <- counts / 2000;



df.events.day.long.10s <- data.frame(time=1:length(events.y$day),
                                volume=events.y$day / max(events.y$day),
                                z=(tempz/10)-1, prob=counts/2 - 1.5);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

setwd("C:/Data/NetworkResearch/Results");
ggsave( "AR-HMM-10State.png", height=6, width=10,
        plot=ggplot(df.events.day.long.10s, aes(x=time)) +
        geom_line(aes(y=volume)) +
        geom_step(aes(y=z), color="blue") +
        geom_line(aes(y=prob), color="darkgreen") +
        labs(x="Time", y="Volume") );




setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

load("results.bombing.interval.3s.long.rdata");

bestSample.bombing.interval.long.3s <- samplerResults.bombing.interval.long.3s$bestSample;

tempz <- bestSample.bombing.interval.long.3s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.interval.long.3s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.1day.3s.long.png", height=5, width=10,
        plot=ggplot(df.bombing.interval.long.3s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );



setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.bombing.interval.5s.long.rdata");

bestSample.bombing.interval.long.5s <- samplerResults.bombing.interval.long.5s$bestSample;

tempz <- bestSample.bombing.interval.long.5s$z;
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==newOrder[i]] <- 100 + i;
}
tempz <- tempz - 100;

df.bombing.interval.long.5s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.1day.5s.long.png", height=5, width=10,
        plot=ggplot(df.bombing.interval.long.5s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.bombing.interval.10s.long.rdata");

bestSample.bombing.interval.long.10s <- samplerResults.bombing.interval.long.10s$bestSample;

tempz <- bestSample.bombing.interval.long.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;


df.bombing.interval.long.10s <- data.frame(time=1:length(bombing1.y$interval),
                                volume=bombing1.y$interval / max(bombing1.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Bombing.1day.10s.long.png", height=5, width=10,
        plot=ggplot(df.bombing.interval.long.10s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.interval.5s.long.rdata");

bestSample.events.interval.long.5s <- samplerResults.events.interval.long.5s$bestSample;

tempz <- bestSample.events.interval.long.5s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;


df.events.interval.long.5s <- data.frame(time=1:length(events.y$interval),
                                volume=events.y$interval / max(events.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.11day.5s.long.png", height=5, width=10,
        plot=ggplot(df.events.interval.long.5s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );



setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.interval.10s.long.rdata");

bestSample.events.interval.long.10s <- samplerResults.events.interval.long.10s$bestSample;

tempz <- bestSample.events.interval.long.10s$z;
temptable <- table(tempz);
newOrder <- order(table(tempz), decreasing=TRUE);
for ( i in 1:length(newOrder) ) {
  tempz[tempz==as.numeric(names(temptable)[newOrder[i]])] <- 100 + i;
}
tempz <- tempz - 100;


df.events.interval.long.10s <- data.frame(time=1:length(events.y$interval),
                                volume=events.y$interval / max(events.y$interval),
                                z=tempz);

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");

ggsave( "Events.11day.10s.long.png", height=5, width=10,
        plot=ggplot(df.events.interval.long.10s, aes(x=time)) +
        geom_line(aes(y=volume), color="blue") +
        geom_step(aes(y=z), color="red") +
        labs(x="Time", y="Volume") );
