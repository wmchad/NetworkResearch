require("bcp");

setwd("c:/Code/NetworkResearch/R/Analysis/");
source("SwitchingAR.r");
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

## Analysis - concatenated events

samplerResults <- NULL;
samplerResults$events <- NULL;
samplerResults$bombing <- NULL;

nBurn <- 2000;
nSamp <- 2000;
verbose <- TRUE;
nAnnounce <- 10;

setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Results");

K <- 3;
hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);

Rprof();
samplerResults.bombing.day.3s <- SwitchingARSampler( hypers, K, bombing1.y$day,
                                                     nBurn, nSamp, verbose=verbose,
                                                     nAnnounce=nAnnounce );
summaryRprof();
Rprof(NULL);

save(samplerResults.bombing.day.3s, file="results.bombing.day.3s.rdata");

samplerResults.bombing.interval.3s <- SwitchingARSampler( hypers, K, bombing1.y$interval,
                                                          nBurn, nSamp, verbose=verbose,
                                                          nAnnounce=nAnnounce );
save(samplerResults.bombing.interval.3s, file="results.bombing.interval.3s.rdata");

samplerResults.events.day.3s <- SwitchingARSampler( hypers, K, events.y$day,
                                                    nBurn, nSamp, verbose=verbose,
                                                    nAnnounce=nAnnounce );
save(samplerResults.events.day.3s, file="results.events.day.3s.rdata");

samplerResults.events.interval.3s <- SwitchingARSampler( hypers, K, events.y$interval,
                                                         nBurn, nSamp, verbose=verbose,
                                                         nAnnounce=nAnnounce );
save(samplerResults.events.interval.3s, file="results.events.interval.3s.rdata");



K <- 5;
hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);

samplerResults.bombing.day.5s <- SwitchingARSampler( hypers, K, bombing1.y$day,
                                                     nBurn, nSamp, verbose=verbose,
                                                     nAnnounce=nAnnounce );
save(samplerResults.bombing.day.5s, file="results.bombing.day.5s.rdata");

samplerResults.bombing.interval.5s <- SwitchingARSampler( hypers, K, bombing1.y$interval,
                                                          nBurn, nSamp, verbose=verbose,
                                                          nAnnounce=nAnnounce );
save(samplerResults.bombing.interval.5s, file="results.bombing.interval.5s.rdata");

samplerResults.events.day.5s <- SwitchingARSampler( hypers, K, events.y$day,
                                                    nBurn, nSamp, verbose=verbose,
                                                    nAnnounce=nAnnounce );
save(samplerResults.events.day.5s, file="results.events.day.5s.rdata");

samplerResults.events.interval.5s <- SwitchingARSampler( hypers, K, events.y$interval,
                                                         nBurn, nSamp, verbose=verbose,
                                                         nAnnounce=nAnnounce );
save(samplerResults.events.interval.5s, file="results.events.interval.5s.rdata");



K <- 10;
hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);

samplerResults.bombing.day.10s <- SwitchingARSampler( hypers, K, bombing1.y$day,
                                                      nBurn, nSamp, verbose=verbose,
                                                      nAnnounce=nAnnounce );
save(samplerResults.bombing.day.10s, file="results.bombing.day.10s.rdata");

samplerResults.bombing.interval.10s <- SwitchingARSampler( hypers, K, bombing1.y$interval,
                                                           nBurn, nSamp, verbose=verbose,
                                                           nAnnounce=nAnnounce );
save(samplerResults.bombing.interval.10s, file="results.bombing.interval.10s.rdata");

samplerResults.events.day.10s <- SwitchingARSampler( hypers, K, events.y$day,
                                                     nBurn, nSamp, verbose=verbose,
                                                     nAnnounce=nAnnounce );
save(samplerResults.events.day.10s, file="results.events.day.10s.rdata");

samplerResults$events$interval.10s <- SwitchingARSampler( hypers, K, events.y$interval,
                                                          nBurn, nSamp, verbose=verbose,
                                                          nAnnounce=nAnnounce );
save(samplerResults.events.interval.10s, file="results.events.interval.10s.rdata");




## Changepoint Detection

bcpResults.events.day <- bcp(events.y$day, w0=1, p0=.0005,
                             return.mcmc=TRUE, burnin=500, mcmc=5000);

bcpstates <- (cumsum(bcpResults.events.day$mcmc.rhos[,2105]) /
              sum(bcpResults.events.day$mcmc.rhos[,2105])) - 1;

bcpnames="1";

df.bcp <- data.frame(Time=1:length(events.y$day), Volume=events.y$day,
                     States=bcpstates,
                     Prob=bcpResults.events.day$posterior.prob/2 - 1);

setwd("C:/Data/NetworkResearch/Results");
save(bcpResults.events.day, file="bcpResults.events.day.rdata");

ggsave("ChangepointResults.png", height=6, width=10,
       plot=ggplot(df.bcp, aes(Time, Volume)) + geom_line() +
       geom_line(aes(y=States), color="blue") +
       geom_line(aes(y=Prob), color="darkgreen"));


df.events.day <- data.frame(time=c(1:288, (1:288) + 320, (1:288) + 640, (1:288) + 960),
                            event=c(rep("inaug1", 288), rep("bombing1", 288),
                              rep("inaug2", 288), rep("bombing2", 288)),
                            volume=events.y$day / max(events.y$day),
                            cpstate=bcpstates - .2,
                            cpnames=bcpnames);

eventStarts <- c(0, 90, 100, 461, 466, 807, 816, 1093, 1095);
eventEnds <- c(90, 100, 320+11*12+9, 466, 807, 816, 1093, 1095, 1250)

eventStarts <- c(0, 91, 118, 462, 466, 815, 854, 1095, 1100);
eventEnds <- c(91, 118, 462, 466, 815, 854, 1095, 1100, 1250)

cpSummaryPlot <- ggplot() +
  annotate("rect", xmin=eventStarts, xmax=eventEnds, ymin=-Inf, ymax=-.2,
           fill=c("green", "#FDDC00", "green", "red", "green",
             "blue", "green", "purple", "green"), alpha=0.4) +
  geom_line(data=df.events.day, aes(x=time, y=volume, group=event), color="blue") +
  geom_step(data=df.events.day, aes(x=time, y=cpstate, group=event)) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  scale_x_continuous(breaks=c(300, 620, 940), minor_breaks=NULL, limits=c(0, 1250)) +
  scale_y_continuous(breaks=NULL)

setwd("C:/Data/NetworkResearch/Results");
setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
ggsave("ChangepointSummaryPlot2.png", height=5, width=10, plot=cpSummaryPlot);




plot(events.y$day, type="l", ylim=c(-1, 1))
lines(bcpstates, type="s", col="blue");
lines(bcpResults.events.day$posterior.prob/2 - 1, col="green");

plot(cumsum(bcpResults.events.day$mcmc.rhos[,51]), type="s")
lines(cumsum(bcpResults.events.day$mcmc.rhos[,52]), type="s")
lines(cumsum(bcpResults.events.day$mcmc.rhos[,550]), type="s", lty=2)


## State plots

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


df.events.day.long.10s <- data.frame(time=c(1:288, (1:288) + 320, (1:288) + 640, (1:288) + 960),
                            event=c(rep("inaug1", 288), rep("bombing1", 288),
                              rep("inaug2", 288), rep("bombing2", 288)),
                            volume=events.y$day / max(events.y$day),
                            state=tempz/10-1);

eventStarts <- c(0, 91, 139, 463, 468, 808, 867, 1095, 1098);
eventEnds <- c(91, 139, 463, 468, 808, 867, 1095, 1098, 1250)

arHmmSummaryPlot <- ggplot() +
  annotate("rect", xmin=eventStarts, xmax=eventEnds, ymin=-Inf, ymax=-.2,
           fill=c("green", "#FDDC00", "green", "red", "green",
             "#FDDC00", "green", "red", "green"), alpha=0.4) +
  geom_line(data=df.events.day.long.10s, aes(x=time, y=volume, group=event), color="blue") +
  geom_step(data=df.events.day.long.10s, aes(x=time, y=state, group=event)) +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank()) +
  scale_x_continuous(breaks=c(300, 620, 940), minor_breaks=NULL, limits=c(0, 1250)) +
  scale_y_continuous(breaks=NULL)

setwd("C:/Data/NetworkResearch/Results");
ggsave( "ArHmm10stateSummaryPlot.png", height=5, width=10,
        plot=arHmmSummaryPlot);


## Month-long stuff
## K <- 3
## hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);
## samplerResults.events.month.3s <- SwitchingARSampler( hypers, K, events.y$month,
##                                                       nBurn, nSamp, verbose=verbose,
##                                                       nAnnounce=nAnnounce );
## save(samplerResults.events.month.3s, file="results.events.month.3s.rdata");
## 
## samplerResults.bombing.month.3s <- SwitchingARSampler( hypers, K, bombing1.y$month,
##                                                        nBurn, nSamp, verbose=verbose,
##                                                        nAnnounce=nAnnounce );
## save(samplerResults.bombing.month.3s, file="results.bombing.month.3s.rdata");
## 
## 
## K <- 5
## hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);
## samplerResults.events.month.5s <- SwitchingARSampler( hypers, K, events.y$month,
##                                                       nBurn, nSamp, verbose=verbose,
##                                                       nAnnounce=nAnnounce );
## save(samplerResults.events.month.5s, file="results.events.month.5s.rdata");
## 
## samplerResults.bombing.month.5s <- SwitchingARSampler( hypers, K, bombing1.y$month,
##                                                        nBurn, nSamp, verbose=verbose,
##                                                        nAnnounce=nAnnounce );
## save(samplerResults.bombing.month.5s, file="results.bombing.month.5s.rdata");
## 
## 
## K <- 10
## hypers <- list(alpha=1/K, a=.1, b=.001, sigma2mu=1, sigma2a=1);
## samplerResults$events$month.10s <- SwitchingARSampler( hypers, K, events.y$month,
##                                                        nBurn, nSamp, verbose=verbose,
##                                                        nAnnounce=nAnnounce );
## save(samplerResults.events.month.10s, file="results.events.month.10s.rdata");
## 
## samplerResults.bombing.month.10s <- SwitchingARSampler( hypers, K, bombing1.y$month,
##                                                         nBurn, nSamp, verbose=verbose,
##                                                         nAnnounce=nAnnounce );
## save(samplerResults.bombing.month.10s, file="results.bombing.month.10s.rdata");
