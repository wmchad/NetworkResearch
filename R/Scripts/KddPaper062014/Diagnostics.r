require(ggplot2);
require(reshape);
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
load("results.events.day.5s.rdata");

bestSample.events.day.5s <- samplerResults.events.day.5s$bestSample;

mus <- sapply(samplerResults.events.day.5s$samples, function(s) {
  s$mu;
});

apply(mus, 1, function(m) {
  c(mean(m), sd(m));
});
##                 1             2            3          4             5
## mean 0.0295853278 -2.259353e-04 9.830124e-03 0.70195385 -4.916023e-02
## var  0.0000325874  2.054474e-07 5.903926e-06 0.03433728  6.902603e-05

ars <- sapply(samplerResults.events.day.5s$samples, function(s) {
  s$a;
});

apply(ars, 1, function(a) {
  c(mean(a), var(a));
});
##                 1           2           3           4            5
## mean 0.9740292338 0.117382098 0.443091754 -0.10679755 0.8842129203
## var  0.0003665594 0.005168502 0.006584884  0.08323778 0.0008397566

sigma2s <- sapply(samplerResults.events.day.5s$samples, function(s) {
  s$sigma2;
});

apply(sigma2s, 1, function(s) {
  c(mean(s), var(s));
});
##                 1            2            3           4            5
## mean 2.837585e-03 7.114374e-05 9.673824e-04 0.055961263 2.175780e-03
## var  1.552354e-07 8.694197e-11 3.938487e-08 0.002348753 2.550316e-07

plot(ars[1,], sigma2s[1,]);
plot(mus[2,]);
plot(mus[3,]);
plot(mus[4,]);
plot(mus[5,]);

fullz <- sapply(samplerResults.events.day.5s$samples, function(s) {
  s$z;
});

RareCounts <- function(z) {
  fullcounts <- table(z);
  rarityCounts <- matrix(ncol=length(fullcounts), nrow=nrow(z), data=0);
  for ( i in 1:ncol(z) ) {
    zcounts <- table(z[,i]);
    for ( j in 1:length(zcounts) ) {
      index <- as.numeric(names(zcounts))[order(zcounts)[j]];
      rarityCounts[z[,i]==index,j] <- rarityCounts[z[,i]==index,j] + 1;
    }
  }
  rarityCounts;
}

rCounts <- RareCounts(fullz);
rProbs <- rCounts / ncol(fullz);

df <- data.frame(time=1:nrow(fullz), volume=events.y$day,
                 rare=rProbs[,1]+1, rare2=rProbs[,2]+1, rare3=rProbs[,3]+1,
                 rare4=rProbs[,4]+1, rare5=rProbs[,5]+1);

ggplot(df, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");

ggplot(df, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare4), color="red");

ggplot(df, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare5), color="red");


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.10s.rdata");

fullz.10s <- sapply(samplerResults.events.day.10s$samples, function(s) {
  s$z;
});

rCounts.10s <- RareCounts(fullz.10s);
rProbs.10s <- rCounts.10s / ncol(fullz.10s);

df.10s <- data.frame(time=1:nrow(fullz), volume=events.y$day,
                     rare=rProbs.10s[,1]+1, rare2=rProbs.10s[,2]+1, rare3=rProbs.10s[,3]+1,
                     rare4=rProbs.10s[,4]+1, rare5=rProbs.10s[,5]+1, rare6=rProbs.10s[,6]+1,
                     rare7=rProbs.10s[,7]+1, rare8=rProbs.10s[,8]+1, rare9=rProbs.10s[,9]+1,
                     rare10=rProbs.10s[,10]+1);

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare4), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare5), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare6), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare7), color="red");


mus.10s <- sapply(samplerResults.events.day.10s$samples, function(s) {
  s$mu;
});

df.mus.10s <- melt(t(mus.10s));

ggplot(df.mus.10s, aes(x=X1, y=value, group=as.factor(X2), color=as.factor(X2))) + geom_line()

plot(mus.10s[1,], type="l", col="red")
plot(mus.10s[2,], type="l", col="red")
plot(mus.10s[3,], type="l", col="red")
plot(mus.10s[4,], type="l", col="red")
plot(mus.10s[5,], type="l", col="red")
plot(mus.10s[6,], type="l", col="red")
plot(mus.10s[7,], type="l", col="red")
plot(mus.10s[8,], type="l", col="red")
plot(mus.10s[9,], type="l", col="red")
plot(mus.10s[10,], type="l", col="red")
plot(mus.10s[1,], mus.10s[2,])

logliks.10s <- sapply(samplerResults.events.day.10s$samples, function(s) {
  s$loglik;
});

plot(logliks.10s, type="l")

logliks.5s <- sapply(samplerResults.events.day.5s$samples, function(s) {
  s$loglik;
});

plot(logliks.5s, type="l")


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.3s.long.rdata");

logliks.3s <- sapply(samplerResults.events.day.long.3s$samples, function(s) {
  s$loglik;
});

plot(logliks.3s, type="l")


fullz.3s <- sapply(samplerResults.events.day.long.3s$samples, function(s) {
  s$z;
});

fullz.3s <- fullz.3s[,400:2000];

rCounts.3s <- RareCounts(fullz.3s);
rProbs.3s <- rCounts.3s / ncol(fullz.3s);

df.3s <- data.frame(time=1:nrow(fullz.3s), volume=events.y$day,
                    rare=rProbs.3s[,1]+1, rare2=rProbs.3s[,2]+1, rare3=rProbs.3s[,3]+1);


ggplot(df.3s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df.3s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df.3s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");


setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.3s.rdata");

fullz.3sb <- sapply(samplerResults.events.day.3s$samples, function(s) {
  s$z;
});

rCounts.3sb <- RareCounts(fullz.3sb);
rProbs.3sb <- rCounts.3sb / ncol(fullz.3sb);

df.3sb <- data.frame(time=1:nrow(fullz.3sb), volume=events.y$day,
                    rare=rProbs.3sb[,1]+1, rare2=rProbs.3sb[,2]+1, rare3=rProbs.3sb[,3]+1);


ggplot(df.3sb, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df.3sb, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df.3sb, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");


## 5s long events
setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.5s.long.rdata");

bestSample.5s <- samplerResults.events.day.long.5s$bestSample;

logliks.5s <- sapply(samplerResults.events.day.long.5s$samples, function(s) {
  s$loglik;
});

plot(logliks.5s, type="l")


fullz.5s <- sapply(samplerResults.events.day.long.5s$samples, function(s) {
  s$z;
});

rCounts.5s <- RareCounts(fullz.5s);
rProbs.5s <- rCounts.5s / ncol(fullz.5s);

df.5s <- data.frame(time=1:nrow(fullz.5s), volume=events.y$day,
                    rare=rProbs.5s[,1]+1, rare2=rProbs.5s[,2]+1, rare3=rProbs.5s[,3]+1,
                    rare4=rProbs.5s[,4]+1, rare5=rProbs.5s[,5]+1 );


ggplot(df.5s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df.5s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df.5s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");

ggplot(df.5s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare4), color="red");

ggplot(df.5s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare5), color="red");



## 10s long events
setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");
load("results.events.day.10s.long.rdata");

bestSample.10s <- samplerResults.events.day.long.10s$bestSample;

logliks.10s <- sapply(samplerResults.events.day.long.10s$samples, function(s) {
  s$loglik;
});

plot(logliks.10s, type="l")


fullz.10s <- sapply(samplerResults.events.day.long.10s$samples, function(s) {
  s$z;
});

fullz.10s <- fullz.10s[,1200:2000];

rCounts.10s <- RareCounts(fullz.10s);
rProbs.10s <- rCounts.10s / ncol(fullz.10s);

df.10s <- data.frame(time=1:nrow(fullz.10s), volume=events.y$day,
                    rare=rProbs.10s[,1]+1, rare2=rProbs.10s[,2]+1, rare3=rProbs.10s[,3]+1,
                    rare4=rProbs.10s[,4]+1, rare5=rProbs.10s[,5]+1 );


ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare2), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare3), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare4), color="red");

ggplot(df.10s, aes(x=time, y=volume)) + geom_line(color="blue") +
  geom_line(aes(y=rare5), color="red");


