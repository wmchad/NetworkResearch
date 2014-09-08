require(ggplot2);

SimulateArProcess <- function( n, sigma2, ar ) {
  innovations <- rnorm(n, 0, sigma2);
  y <- rep(0, n+1);
  for ( i in 2:(n+1) ) {
    y[i] <- ar*y[i-1] + innovations[i-1];
  }
  y[-1];
}

SimulateArProcessWithEvent <- function( n, sigma2, ar, eStart, eEnd, eMagnitude, eAr, emu, esigma2 ) {
  innovations <- rnorm(n, 0, sigma2);
  innovations[eStart:eEnd] <- rnorm(eEnd-eStart+1, emu, esigma2);
  y <- rep(0, n+1);
  for ( i in 1:n ) {
    arCur <- ar;
    if ( i > eStart && i <= eEnd ) {
      arCur <- eAr;
    }    
    y[i+1] <- arCur*y[i] + innovations[i];
    if ( i == eStart ) {
      y[i+1] <- y[i+1] + eMagnitude;
    }
  }
  y;
}

testAr <- SimulateArProcess( 1000, 1, .6 );

## Bombing-type event
testEventAr <- SimulateArProcessWithEvent( 288, .5, .5, 142, 150, 5, .9, 0, .5);
## Festival-type event
testEvent2Ar <- SimulateArProcessWithEvent( 288, .5, .7, 142, 200, 0, 1, .1, .1 );



SimulateArHmm <- function( states, params ) {
  n <- length(states);
  innovations <- rnorm(n, params$mu[states], sqrt(params$sigma2[states]));
  y <- rep(0, n+1);
  for ( i in 1:n ) {
    y[i+1] <- params$ar[states[i]] * y[i] + innovations[i];
  }
  y[-1];
}

states1 <- c(rep(1,200), rep(2,10), rep(1,200));
params1 <- list(mu=c(0,3), sigma2=c(1,.5)^2, ar=c(.2,.7));

hmmAr <- data.frame(time=1:length(states1), volume=SimulateArHmm( states1, params1 ));

ggplot( hmmAr, aes(x=time, y=volume) ) + geom_line()

states2 <- c(rep(1,100), rep(2,50), rep(1,100));
params2 <- list(mu=c(0,.1), sigma2=c(.3,.1)^2, ar=c(.5,.99));
hmmAr2 <- data.frame(time=1:length(states2), volume=SimulateArHmm( states2, params2 ));

ggplot( hmmAr2, aes(x=time, y=volume) ) + geom_line();

states3 <- c(rep(1,200), rep(2,100), rep(1,200));
params3 <- list(mu=c(0,10), sigma2=c(1,2)^2, ar=c(0,.1));
hmmAr3 <- data.frame(time=1:length(states3), volume=SimulateArHmm( states3, params3 ));

ggplot( hmmAr3, aes(x=time, y=volume) ) + geom_line()

states4 <- c(rep(1,100), rep(2,100), rep(3,100), rep(1,100));
params4 <- list(mu=c(0,.1,.01), sigma2=c(.3,.05,.05)^2, ar=c(.5,.95,.98));
hmmAr4 <- data.frame(time=1:length(states4), volume=SimulateArHmm( states4, params4 ));

ggplot( hmmAr4, aes(x=time, y=volume) ) + geom_line();

states5 <- c(rep(1,100), rep(2,10), rep(3,20), rep(1,100));
params5 <- list(mu=c(0,3,0), sigma2=c(1,.5,1)^2, ar=c(.2,.7,.9));
hmmAr5 <- data.frame(time=1:length(states5), volume=SimulateArHmm( states5, params5 ));

ggplot( hmmAr5, aes(x=time, y=volume) ) + geom_line()

states6 <- c(rep(1,200), rep(2,100), rep(1,200));
params6 <- list(mu=c(0,5), sigma2=c(1,2)^2, ar=c(0,.5));
hmmAr6 <- data.frame(time=1:length(states6), volume=SimulateArHmm( states6, params6 ));

ggplot( hmmAr6, aes(x=time, y=volume) ) + geom_line()

states7 <- c(rep(1,100), rep(2,100), rep(3,100), rep(1,100));
params7 <- list(mu=c(0,.1,.01), sigma2=c(.3,.05,.05)^2, ar=c(.2,.95,.98));
hmmAr7 <- data.frame(time=1:length(states7), volume=SimulateArHmm( states7, params7 ));

ggplot( hmmAr7, aes(x=time, y=volume) ) + geom_line();

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 3;

hmmSampler4 <- SwitchingARSampler( hypers, K, hmmAr4$volume, 1000, 1000, verbose=TRUE );
hmmSampler5 <- SwitchingARSampler( hypers, K, hmmAr5$volume, 1000, 1000, verbose=TRUE );
## do this one hmmSampler6 <- SwitchingARSampler( hypers, K, hmmAr6$volume, 1000, 1000, verbose=TRUE );
hmmSampler7 <- SwitchingARSampler( hypers, K, hmmAr7$volume, 1000, 1000, verbose=TRUE );

hmmSampler1 <- SwitchingARSampler( hypers, K, hmmAr$volume, 1000, 1000, verbose=TRUE );
hmmSampler2 <- SwitchingARSampler( hypers, K, hmmAr2$volume, 1000, 1000, verbose=TRUE );
hmmSampler3 <- SwitchingARSampler( hypers, K, hmmAr3$volume, 1000, 1000, verbose=TRUE );


GetBestState <- function( sampler, y ) {
  nSamp <- length(sampler$samples);
  logliks <- sapply(sampler$samples, StateLogLik, y);
  sampler$samples[[(1:nSamp)[logliks==max(logliks)]]];
}

GetBestStateDataFrame <- function( sampler, y, z ) {
  data.frame(time=1:length(y), volume=y,
             z=GetBestState(sampler, y)$z, truez=z);
}

bestState4 <- GetBestState(hmmSampler4, hmmAr4$volume);

df4 <- GetBestStateDataFrame(hmmSampler4, hmmAr4$volume, states4);
df5 <- GetBestStateDataFrame(hmmSampler5, hmmAr5$volume, states5);
df6 <- GetBestStateDataFrame(hmmSampler6, hmmAr6$volume, states6);
df7 <- GetBestStateDataFrame(hmmSampler7, hmmAr7$volume, states7);

ggplot(df4, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume")

ggplot(df5, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume")

ggplot(df6, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume")

ggplot(df7, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume")


## Long state sequence for better identifiability
statesLong <- rep(c(rep(1,100), rep(2,100), rep(3,100), rep(1,100)), 5);
paramsLong <- list(mu=c(0,.1,.01), sigma2=c(.3,.05,.05)^2, ar=c(.2,.95,.98));
hmmArLong <- data.frame(time=1:length(statesLong),
                        volume=SimulateArHmm( statesLong, paramsLong ));

hypers <- list(alpha=1, a=.1, b=.001, sigma2mu=1, sigma2a=1);
K <- 3;
hmmSamplerLong <- SwitchingARSampler( hypers, K, hmmArLong$volume, 1000, 1000, verbose=TRUE );

bestStateLong <- GetBestState(hmmSamplerLong, hmmArLong$volume);

dfLong <- GetBestStateDataFrame(hmmSamplerLong, hmmArLong$volume, statesLong);

ggplot(dfLong, aes(x=time)) + geom_line(aes(y=volume), color="red") +
       geom_step(aes(y=truez), color="darkgreen", lwd=2) +
       geom_step(aes(y=z), color="yellow") +
       labs(x="Time", y="Volume")


inferredParams <- list(mu=bestStateLong$mu,
                       sigma2=bestStateLong$sigma2,
                       ar=bestStateLong$a)
## $mu
## -0.003928100  0.002858735  0.103323262

## $sigma2
## 0.078730225 0.002475452 0.002307767

## $ar
## 0.1530550 0.9820847 0.9506628


simulatedData <- SimulateArHmm( bestStateLong$z, inferredParams )

dfSim <- data.frame(time=1:length(hmmArLong$volume), y=hmmArLong$volume,
                    yhat=simulatedData-2);

ggplot(dfSim, aes(x=time, y=y)) + geom_line(color="blue") +
  geom_line(aes(y=yhat), color="red");
