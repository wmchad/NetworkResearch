source("c:/Code/NetworkResearch/R/Analysis/StickyHdpHmm/Sampler.r");

SimulateData <- function( T, transMat, params ) {
  states <- rep(1, T);
  for ( t in 2:T ) {
    states[t] <- sample(nrow(transMat), 1, prob=transMat[states[t-1],]);
  }
  innovations <- rnorm(T, params$mu[states], sqrt(params$sigma2[states]));
  y <- rep(0, T+1);
  for ( i in 1:T ) {
    y[i+1] <- params$ar[states[i]] * y[i] + innovations[i];
  }
  data.frame(z=states, y=y[-1], stringsAsFactors=F);
}

transMat <- matrix(nrow=3, ncol=3, data=.005) + diag(.985, 3);
params <- list(mu=c(0,1,-.5), sigma2=c(1,.5, .1), ar=c(.9, 0, -.9));
T <- 2000;
data <- SimulateData( T, transMat, params );

hypers <- list(a1=1, b1=.01, a2=1, b2=.01, a3=1.2, b3=1, c=100, d=1,
               sigma2mu=25, sigma2a=25);
L <- 3;
sampler <- HdpHmmSampler( hypers, L, data$y, 2000, 2000, TRUE, 100 );

logliks <- unlist(lapply(sampler$samples, StateLogLik, y=data$y));

bestState <- sampler$bestSample;

ztemp <- bestState$z;
ztemp[ztemp==2] <- 11;
ztemp[ztemp==3] <- 12;
ztemp[ztemp==1] <- 13;
ztemp <- ztemp - 10;

plot(data$z, col="darkgreen", type="s", lwd=4);
lines(ztemp, col="orange", type="s");



L <- 10;
sampler.10s <- HdpHmmSampler( hypers, L, data$y, 2000, 2000, TRUE, 100 );

bestState.10s <- sampler.10s$bestSample;

plot(data$z, col="darkgreen", type="s", lwd=4);
lines(bestState.10s$z, col="orange", type="s");




















res <- NULL;
res$alpha <- unlist(lapply(sampler.test$samples, function(s){s$alpha}));
res$kappa <- unlist(lapply(sampler.test$samples, function(s){s$kappa}));
res$gamma <- unlist(lapply(sampler.test$samples, function(s){s$gamma}));


res.bi <- NULL;
res.bi$alpha <- unlist(lapply(sampler.test$burnin, function(s){s$alpha}));
res.bi$kappa <- unlist(lapply(sampler.test$burnin, function(s){s$kappa}));
res.bi$gamma <- unlist(lapply(sampler.test$burnin, function(s){s$gammao}));
res.bi$z <- t(sapply(sampler.test$burnin, function(s){s$z}));

res.bi$mu <- t(sapply(sampler.test$burnin, function(s){s$mu}));
res.bi$sigma2 <- t(sapply(sampler.test$burnin, function(s){s$sigma2}));
res.bi$a <- t(sapply(sampler.test$burnin, function(s){s$a}));

zs.state1 <- res$z[,data$z==1]
zs.state2 <- res$z[,data$z==2]
zs.state3 <- res$z[,data$z==3]

ztbl <- apply(res$z, 2, table);
zcnt <- sapply(1:T, function(t) { sum(res$z[,t] == data$z[t]); })
maxzs <- unlist(sapply(ztbl, function(tbl) { (1:length(tbl))[tbl == max(tbl)]; }));

curState <- sampler$samples[[1000]];
finalStateData <- data.frame(time=1:2000, y=data$y, z=data$z, z.samp=curState$z);

setwd("~/Dropbox/Chad/UW/NetworkResearch/Code/R/StickyHdpHmm/Plots/");
ggsave("TestData.pdf",
       ggplot(finalStateData, aes(x=time, y=y)) +
       geom_line() + geom_line(aes(y=z), color="red"));

ggsave("InferredStates.pdf",
       ggplot(finalStateData, aes(x=time, y=z.samp)) +
       geom_point() + geom_line(aes(y=z), color="red"))

logliks <- unlist(lapply(sampler$samples, StateLogLik, y=data$y));
logliks.burnin <- unlist(lapply(sampler$burnin, StateLogLik, y=data$y));
finalStateData$logliks <- logliks;

ggsave("logliks.pdf", ggplot(finalStateData, aes(x=time, y=logliks)) + geom_line());

trueState <- curState
trueState$z <- data$z
trueState$mu <- params$mu
trueState$a <- params$a
trueState$sigma2 <- params$sigma2
trueState$Pi <- transMat

StateLogLik(trueState, data$y)
## 92.40475

bestIndex <- 32

bestwindow <- sampler$burnin[32:100]

window.mu1 <- unlist(lapply(bestwindow, function(state){state$mu[1];}));
window.mu2 <- unlist(lapply(bestwindow, function(state){state$mu[2];}));
window.mu3 <- unlist(lapply(bestwindow, function(state){state$mu[3];}));

window.a1 <- unlist(lapply(bestwindow, function(state){state$a[1];}));
window.a2 <- unlist(lapply(bestwindow, function(state){state$a[2];}));
window.a3 <- unlist(lapply(bestwindow, function(state){state$a[3];}));

window.sigma21 <- unlist(lapply(bestwindow, function(state){state$sigma2[1];}));
window.sigma22 <- unlist(lapply(bestwindow, function(state){state$sigma2[2];}));
window.sigma23 <- unlist(lapply(bestwindow, function(state){state$sigma2[3];}));

plot(data$y, type="l")
lines(data$z, col="red", lwd=2)
lines(bestState$z - .5, col="blue", lwd=2)










sampler.test <- DebugHdpHmmSampler( hypers, L, data$y, 1000, 1000, data$z, params, transMat );
logliks.burnin <- unlist(lapply(sampler.test$burnin, StateLogLik, y=data$y));

curState <- sampler.test$samples[[1000]];
bestState <- sampler.test$samples[[(1:1000)[logliks==max(logliks)]]];
