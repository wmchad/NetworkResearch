setwd("c:/Code/NetworkResearch/R/Analysis/");
source("SwitchingAR.r");

transMat <- matrix(nrow=3, ncol=3, data=.005) + diag(.985, 3);
params <- list(mu=c(0,1,-.5), sigma2=c(1,.5, .1), ar=c(.9, 0, -.9));
T <- 2000;
data <- SimulateData( T, transMat, params );

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=25, sigma2a=1);
K <- 3;
sampler <- SwitchingARSampler( hypers, K, data$y, 1000, 1000 );

test.logliks <- sapply(sampler$samples, StateLogLik, data$y);
test.lastState <- sampler$samples[[1000]];
test.bestState <- sampler$samples[[(1:1000)[test.logliks==max(test.logliks)]]];



res <- NULL;
res$mu <- t(sapply(sampler$samples, function(s){s$mu;}));
res$a <- t(sapply(sampler$samples, function(s){s$a;}));
res$sigma2 <- t(sapply(sampler$samples, function(s){s$sigma2;}));

t(apply(res$mu, 2, quantile, c(0.05, 0.5, 0.95) ));
##            5%         50%         95%
## 1 -0.03020828  0.03096338  0.09561287
## 2  0.95674126  1.04683909  1.14091357
## 3 -0.52091144 -0.49878811 -0.47690204

t(apply(res$a, 2, quantile, c(0.05, 0.5, 0.95) ));
##           5%         50%         95%
## 1  0.8607087  0.88817686  0.91477374
## 2 -0.1140120 -0.03710214  0.03305313
## 3 -0.9277825 -0.90723111 -0.88746396

t(apply(res$sigma2, 2, quantile, c(0.05, 0.5, 0.95) ));
##           5%       50%       95%
## 1 1.00433631 1.0976736 1.1867900
## 2 0.46611774 0.5224723 0.5879557
## 3 0.09530176 0.1038783 0.1141052


setwd("c:/Code/NetworkResearch/R/Functions/Data/");
source("TowerFunctions.r");

towerCalls <- GetTowerAggregatedCalls( 790, 11, 12 );

day6Calls <- towerCalls[towerCalls$Day == 6,];

tower.y <- day6Calls$TotalDiffCalls;
tower.y <- tower.y / max(tower.y);

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=25, sigma2a=1);
K <- 3;
towerSampler <- SwitchingARSampler( hypers, K, tower.y, 1000, 1000 );

bombing.logliks <- sapply(towerSampler$samples, StateLogLik, tower.y);
bombing.lastState <- towerSampler$samples[[1000]];
bombing.bestState <- towerSampler$samples[[(1:1000)[bombing.logliks==max(bombing.logliks)]]];

lastState <- towerSampler$samples[[100]];
lastState$mu;
## -0.826107365  0.002975287  1.326265437
lastState$a;
## 0.9611456  0.9070293 -1.1945118
lastState$sigma2
## 0.238970886 0.009779379 0.409485796

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 5;
towerSampler <- SwitchingARSampler( hypers, K, tower.y, 1000, 1000 );
lastState <- towerSampler$samples[[1000]];

logliks <- sapply(towerSampler$samples, StateLogLik, tower.y);
logliks.burnin <- sapply(towerSampler$burnin, StateLogLik, tower.y);

table(lastState$z);
##   3   5 
## 287   1

plot(lastState$z, type="s", col="blue", ylim=range(c(lastState$z, tower.y)));
lines(tower.y, col="red");


towerRes <- NULL;
towerRes$mu <- t(sapply(towerSampler$samples, function(s){s$mu;}));
towerRes$a <- t(sapply(towerSampler$samples, function(s){s$a;}));
towerRes$sigma2 <- t(sapply(towerSampler$samples, function(s){s$sigma2;}));

towerRes$zs <- sapply(towerSampler$samples, function( sample ) {
  sample$z;
});

towerRes$zdist <- apply(towerRes$zs, 1, function(z){
  table(c(z,1:K))-1;
});

t(apply(towerRes$mu, 2, quantile, c(0.05, 0.5, 0.95) ));
##            5%          50%         95%
## 1 -1.36716616  0.267131267 1.630523690
## 2 -1.37074974  0.275254127 1.548581450
## 3 -0.01449559 -0.004885517 0.005079538
## 4 -1.58435167  0.237588915 1.628087115
## 5 -1.43015766  0.206070830 1.632638913
t(apply(towerRes$a, 2, quantile, c(0.05, 0.5, 0.95) ));
##         5%         50%       95%
## -1.5757336 -0.01604837 1.7146972
## -1.6277962 -0.02951822 1.6268715
##  0.8412921  0.89208488 0.9475007
## -1.6008186  0.03956913 1.7294693
## -1.5823307  0.06209432 1.5570906
t(apply(towerRes$sigma2, 2, quantile, c(0.05, 0.5, 0.95) ));
##          5%         50%       95%
## 0.153565634 0.368368524 1.1690560
## 0.152208979 0.360212814 1.1292824
## 0.008512079 0.009681657 0.0111277
## 0.152676055 0.373519448 1.2239356
## 0.158210283 0.367982557 1.2000167



month.y <- towerCalls$TotalDiffCalls[towerCalls$Day <= 10];
month.y <- month.y/max(month.y);
hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 5;
towerSampler.month <- SwitchingARSampler( hypers, K, month.y, 1000, 1000 );
lastState.month <- towerSampler.month$samples[[1000]];

month.logliks <- sapply(towerSampler.month$samples, StateLogLik, month.y);
month.lastState <- towerSampler.month$samples[[1000]];
month.bestState <- towerSampler.month$samples[[(1:1000)[month.logliks==max(month.logliks)]]];

plot(lastState.month$z, type="s", col="blue", ylim=range(c(month.y, lastState.month$z)));
lines(month.y+3, col="red")

fullmonth.y <- towerCalls$TotalDiffCalls;
fullmonth.y <- fullmonth.y/max(fullmonth.y);
hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 5;
towerSampler.fullmonth <- SwitchingARSampler( hypers, K, fullmonth.y, 1000, 1000, verbose=TRUE );
lastState.fullmonth <- towerSampler.fullmonth$samples[[1000]];

plot(lastState.fullmonth$z, type="s", col="blue", ylim=range(c(fullmonth.y, lastState.fullmonth$z)));
lines(fullmonth.y+3, col="red")



## multiple concatenated events

bombingCalls <- GetTowerAggregatedCalls( 790, 11, 12 );
inaug1Calls <- GetTowerAggregatedCalls( 183, 11, 12 );
inaug2Calls <- GetTowerAggregatedCalls( 261, 11, 7 );

bombing.y <- bombingCalls$TotalDiffCalls[bombingCalls$Day == 6];
inaug1.y <- inaug1Calls$TotalDiffCalls[inaug1Calls$Day == 15];
inaug2.y <- inaug2Calls$TotalDiffCalls[inaug2Calls$Day == 23];

events.y <- c(inaug1.y, bombing.y, inaug2.y);
events.y <- events.y/max(events.y);
hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 3;
eventSampler <- SwitchingARSampler( hypers, K, events.y, 1000, 1000 );

event.logliks <- sapply(eventSampler$samples, StateLogLik, events.y);
event.lastState <- eventSampler$samples[[1000]];
event.bestState <- eventSampler$samples[[(1:1000)[event.logliks==max(event.logliks)]]];
event.worstState <- eventSampler$samples[[(1:1000)[event.logliks==min(event.logliks)]]];

plot(event.lastState$z, type="s", col="blue", ylim=range(c(events.y, event.lastState$z)));
lines(events.y, col="red")

plot(event.worstState$z, type="s", col="blue", ylim=range(c(events.y, event.worstState$z)));
lines(events.y, col="red")

plot(event.bestState$z, type="s", col="blue", ylim=range(c(events.y, event.bestState$z)));
lines(events.y, col="red")

event.bestState$mu;
## 0.001106768 0.541640939 0.224323261 
event.bestState$a;
## 0.9071438 1.6923034 0.5329784
event.bestState$sigma2;
## 0.004506838 0.226492903 0.190373538


events2.y <- c(inaug1.y/max(inaug1.y),
              bombing.y/max(bombing.y),
              inaug2.y/max(inaug2.y))

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 3;
eventSampler2 <- SwitchingARSampler( hypers, K, events2.y, 1000, 1000, verbose=TRUE );

event2.logliks <- sapply(eventSampler2$samples, StateLogLik, events2.y);
event2.lastState <- eventSampler2$samples[[1000]];
event2.bestState <- eventSampler2$samples[[(1:1000)[event2.logliks==max(event2.logliks)]]];
event2.worstState <- eventSampler2$samples[[(1:1000)[event2.logliks==min(event2.logliks)]]];

plot(event2.lastState$z, type="s", col="blue", ylim=range(c(events2.y, event2.lastState$z)));
lines(events2.y, col="red")

plot(event2.worstState$z, type="s", col="blue", ylim=range(c(events2.y, event2.worstState$z)));
lines(events2.y, col="red")

plot(event2.bestState$z, type="s", col="blue", ylim=range(c(events2.y, event2.bestState$z)));
lines(events2.y, col="red")

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 5;
eventSampler5state <- SwitchingARSampler( hypers, K, events.y, 1000, 1000 );
eventSampler5state2 <- SwitchingARSampler( hypers, K, events2.y, 1000, 1000 );

event5State.logliks <- sapply(eventSampler5state$samples, StateLogLik, events.y);
event5State.lastState <- eventSampler5state$samples[[1000]];
event5State.bestState <-
  eventSampler5state$samples[[(1:1000)[event5State.logliks==max(event5State.logliks)]]];
event5State.worstState <-
  eventSampler5state$samples[[(1:1000)[event5State.logliks==min(event5State.logliks)]]];

plot(event5State.lastState$z, type="s", col="blue", ylim=range(c(events.y, event5State.lastState$z)));
lines(events.y, col="red")

plot(event5State.worstState$z, type="s", col="blue", ylim=range(c(events.y, event5State.worstState$z)));
lines(events.y, col="red")

plot(event5State.bestState$z, type="s", col="blue", ylim=range(c(events.y, event5State.bestState$z)));
lines(events.y, col="red")

event5State2.logliks <- sapply(eventSampler5state2$samples, StateLogLik, events2.y);
event5State2.lastState <- eventSampler5state2$samples[[1000]];
event5State2.bestState <-
  eventSampler5state2$samples[[(1:1000)[event5State2.logliks==max(event5State2.logliks)]]];
event5State2.worstState <-
  eventSampler5state2$samples[[(1:1000)[event5State2.logliks==min(event5State2.logliks)]]];

plot(event5State2.lastState$z, type="s", col="blue", ylim=range(c(events2.y, event5State2.lastState$z)));
lines(events2.y, col="red")

plot(event5State2.worstState$z, type="s", col="blue", ylim=range(c(events2.y, event5State2.worstState$z)));
lines(events2.y, col="red")

plot(event5State2.bestState$z, type="s", col="blue", ylim=range(c(events2.y, event5State2.bestState$z)));
lines(events2.y, col="red")



df.test <- data.frame(time=1:length(data$y), volume=data$y,
                         z=test.bestState$z+5, truez=data$z+5);

df.bombing <- data.frame(time=1:length(tower.y), volume=tower.y,
                         z=(bombing.bestState$z+1)/2);

df.10day <- data.frame(time=1:length(month.y), volume=month.y,
                         z=month.bestState$z-2);

df.events <- data.frame(time=1:length(events.y), volume=events.y,
                         z=event.bestState$z);

df.events2 <- data.frame(time=1:length(events2.y), volume=events2.y,
                         z=event2.bestState$z);

df.events5state2 <- data.frame(time=1:length(events2.y), volume=events2.y,
                         z=event5State2.bestState$z);

require(ggplot2);

setwd("C:/Plots/NetworkResearch/EventDetection/SwitchingAR");

ggsave("SimulatedDataResults-3state.pdf", height=7, width=12,
       plot=ggplot(df.test, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume"));

ggsave("BombingResults-3state.pdf", height=7, width=12,
       plot=ggplot(df.bombing, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=z), color="blue") +
       labs(x="Time", y="Volume"));

ggsave("Bombing10dayResults-3state.pdf", height=7, width=12,
       plot=ggplot(df.10day, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=z), color="blue") +
       labs(x="Time", y="Volume"));

ggsave("MultiEventResults-3state.pdf", height=7, width=12,
       plot=ggplot(df.events, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=z), color="blue") +
       labs(x="Time", y="Volume"));

ggsave("MultiEvent2Results-3state.pdf", height=7, width=12,
       plot=ggplot(df.events2, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=z), color="blue") +
       labs(x="Time", y="Volume"));

ggsave("MultiEvent2Results-5state.pdf", height=7, width=12,
       plot=ggplot(df.events5state2, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=z), color="blue") +
       labs(x="Time", y="Volume"));








hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=25, sigma2a=1);
K <- 3;
towerSampler <- SwitchingARSampler( hypers, K, tower.y, 1000, 1000, verbose=TRUE );
## finished
towerSampler.month <- SwitchingARSampler( hypers, K, month.y, 1000, 1000, verbose=TRUE );
towerSampler.fullmonth <- SwitchingARSampler( hypers, K, fullmonth.y, 1000, 1000, verbose=TRUE );
eventSampler <- SwitchingARSampler( hypers, K, events.y, 1000, 1000, verbose=TRUE );
## finished
eventSampler2 <- SwitchingARSampler( hypers, K, events2.y, 1000, 1000, verbose=TRUE );
## fimished

K <- 5;
towerSampler.5s <- SwitchingARSampler( hypers, K, tower.y, 1000, 1000, verbose=TRUE );
towerSampler.month.5s <- SwitchingARSampler( hypers, K, month.y, 1000, 1000, verbose=TRUE );
towerSampler.fullmonth.5s <- SwitchingARSampler( hypers, K, fullmonth.y, 1000, 1000, verbose=TRUE );
## finished
eventSampler.5s <- SwitchingARSampler( hypers, K, events.y, 1000, 1000, verbose=TRUE );
## finished
eventSampler2.5s <- SwitchingARSampler( hypers, K, events2.y, 1000, 1000, verbose=TRUE );

fullmonth.5s.logliks <- sapply(towerSampler.fullmonth.5s$samples, StateLogLik, fullmonth.y);
fullmonth.5s.lastState <- towerSampler.fullmonth.5s$samples[[1000]];
fullmonth.5s.bestState <- towerSampler.fullmonth.5s$samples[[(1:1000)[fullmonth.5s.logliks==max(fullmonth.5s.logliks)]]];

plot(fullmonth.5s.bestState$z, type="s", col="blue", ylim=range(c(fullmonth.y, fullmonth.5s.bestState$z)));
lines(fullmonth.y+3, col="red")


events.5s.logliks <- sapply(eventSampler.5s$samples, StateLogLik, events.y);
events.5s.lastState <- eventSampler.5s$samples[[1000]];
events.5s.bestState <- eventSampler.5s$samples[[(1:1000)[events.5s.logliks==max(events.5s.logliks)]]];

plot(events.5s.bestState$z, type="s", col="blue", ylim=range(c(events.y, events.5s.bestState$z)));
lines(events.y+3, col="red")



bombing.y.trimmed <- bombingCalls$TotalDiffCalls[bombingCalls$Day == 6 &
                                                 bombingCalls$DayPeriod >= 80 &
                                                 bombingCalls$DayPeriod <= 230];
inaug1.y.trimmed <- inaug1Calls$TotalDiffCalls[inaug1Calls$Day == 15 &
                                                 bombingCalls$DayPeriod >= 50 &
                                                 bombingCalls$DayPeriod <= 230];
inaug2.y.trimmed <- inaug2Calls$TotalDiffCalls[inaug2Calls$Day == 23 &
                                                 bombingCalls$DayPeriod >= 80 &
                                                 bombingCalls$DayPeriod <= 260];

events.y.trimmed <- c(inaug1.y.trimmed, bombing.y.trimmed, inaug2.y.trimmed);
events.y.trimmed <- events.y.trimmed/max(events.y.trimmed);

events2.y.trimmed <- c(inaug1.y.trimmed/max(inaug1.y.trimmed),
                       bombing.y.trimmed/max(bombing.y.trimmed),
                       inaug2.y.trimmed/max(inaug1.y.trimmed));

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=25, sigma2a=1);
K <- 3;
eventSampler.trimmed <- SwitchingARSampler( hypers, K, events.y.trimmed, 1000, 1000, verbose=TRUE );

events.trimmed.logliks <- sapply(eventSampler.trimmed$samples, StateLogLik, events.y.trimmed);
events.trimmed.lastState <- eventSampler.trimmed$samples[[1000]];
events.trimmed.bestState <- eventSampler.trimmed$samples[[(1:1000)[events.trimmed.logliks==max(events.trimmed.logliks)]]];

plot(events.trimmed.bestState$z, type="s", col="blue", ylim=range(c(events.y.trimmed, events.trimmed.bestState$z)));
lines(events.y.trimmed, col="red")


burnin.logliks <- sapply(eventSampler.trimmed$burnin, StateLogLik, events.y.trimmed);
