require(MCMCpack);

source("c:/Code/NetworkResearch/R/Analysis/SwitchingARrcpp.r");

## 6/13/2014
## Block Gibbs sampler for switching AR process model


##################################################
## Algorithm
##################################################

## Data: y_1, ..., y_T
## Hidden States: z_{1:T} in {1, ..., K}
## Each state k has parameters
##   pi_k: transition probabilities from state k
##   mu_k: state mean impulse
##   a_k: state auto-regression parameter
##   sigma^2: state innovation variance
## n_jk = number of transitions from state j to state k

## Initialize parameters:
##   K = 3
##   pi_k ~ Dirichlet(alpha), k = 1, ..., K
##   mu_k ~ N(0, sigma^2_mu), k = 1, ..., K
##   a_k ~ N(0, sigma^2_a), k = 1, ..., K
##   sigma^2_k ~ IG(a, b), k = 1, ..., K

## Block sample z_{1:T} using backwards-forwards algorithm

## Update pi for k = 1, ..., K
##   pi_k ~ Dirichlet( alpha + n_k1, ..., alpha + n_kK )

## Update a, mu, sigma^2

InitializeState <- function( hypers, K, T ) {
  sampler = list(hypers=hypers, K=K, T=T);
  sampler$Pi <- matrix(nrow=K, ncol=K);
  for ( i in 1:K ) {
    sampler$Pi[i,] <- rdirichlet(1, rep(hypers$alpha, K));
  }
  sampler$mu <- rnorm(K, 0, sqrt(hypers$sigma2mu));
  sampler$a <- rnorm(K, 0, sqrt(hypers$sigma2a));
  sampler$sigma2 <- rinvgamma(K, hypers$a, hypers$b);
  sampler;
}

## Initializes the state parameters (mu and sigma^2)
## from the data
InitializeStateSmart <- function( hypers, K, T, y ) {
  sampler = list(hypers=hypers, K=K, T=T);
  sampler$Pi <- matrix(nrow=K, ncol=K);
  for ( i in 1:K ) {
    sampler$Pi[i,] <- rdirichlet(1, rep(hypers$alpha, K));
  }
  sampler$mu <- sample(y, K);
  sampler$a <- rnorm(K, 0, sqrt(hypers$sigma2a));
  subsamples <- sample(length(y)-20, K);
  sampler$sigma2 <- sapply(subsamples, function(s) {
    var(y[s:(s+20)]);
  });
  sampler;
}

ComputeLikelihoods <- function( curState, y ) {
  lkl <- cpp.GetLikelihoods(length(y), curState$K,
                             y, curState$mu,
                             curState$a, curState$sigma2);
  lkl <- lkl - max( -300, mean(lkl));
  curState$likelihoods <- exp(lkl);
  curState;
}

CanUpdateZ <- function( curState ) {
  fwdProb <- cpp.GetForwardProbabilities(curState$T, curState$K,
                                         curState$Pi, curState$likelihoods)[-1,];
  sum(is.nan(fwdProb)) == 0;
}

Update.z <- function( curState ) {
  fwdProb <- cpp.GetForwardProbabilities(curState$T, curState$K,
                                         curState$Pi, curState$likelihoods)[-1,];
  n <- matrix(nrow=curState$K, ncol=curState$K, data=0);
  z <- rep(0, curState$T);
  if (sum(fwdProb[1,]) == 0) {
    print("sampling z[1] uniformly");
    z[1] <- sample(curState$K, 1);    
  } else {
    z[1] <- sample(curState$K, 1, prob=fwdProb[1,]);
  }
  for ( t in 2:curState$T ) {
    if (sum(curState$Pi[z[t-1],] * fwdProb[t,]) == 0) {
      print("sampling z uniformly");
      z[t] <- sample(curState$K, 1);    
    }
    else {
      z[t] <- sample(curState$K, 1, prob=curState$Pi[z[t-1],] * fwdProb[t,]);
    }
    n[z[t-1],z[t]] <- n[z[t-1],z[t]] + 1;
  }
  curState$z <- z;
  curState$n <- n;
  curState;
}

Update.Pi <- function( curState ) {
  PiNext <- matrix(nrow=curState$K, ncol=curState$K);
  for ( i in 1:curState$K ) {
    delta <- rep(0, curState$K);
    delta[i] <- 1;
    PiNext[i,] <- rdirichlet(1, curState$hypers$alpha + curState$n[i,]);
  }
  curState$Pi <- PiNext;
  curState;
}

Update.theta <- function( curState, y ) {
  ## Prep
  nsums <- table(c(curState$z, 1:curState$K)) - 1;
  nThetaSamples <- 50;
  normSamples <- rnorm(2 * curState$K * nThetaSamples, 0, 1);
  startRand <- 0;
  ssy <- cpp.GetStateSSYs( length(y), curState$K, y, curState$z);
  ## mu update
  for ( i in 1:nThetaSamples ) {
    curState$mu <- Update.mu( curState, y, nsums, normSamples[(1:curState$K) + startRand] );
    startRand <- startRand + curState$K;
    curState$sigma2 <- Update.sigma2( curState, y, nsums );
    curState$a <- Update.a( curState, y, normSamples[(1:curState$K) + startRand], ssy );
    startRand <- startRand + curState$K;
  }
  curState;
}

Update.mu <- function( curState, y, nsums, normSamples ) {
  rt <- cpp.GetStateArResids( length(y), curState$K, y,
                               curState$z, curState$a );
  normSamples * sqrt(curState$sigma2 / (nsums + curState$sigma2 / curState$hypers$sigma2mu)) + 
        rt / (nsums + curState$sigma2 / curState$hypers$sigma2mu);
}

Update.a <- function( curState, y, normSamples, ssy ) {
  rt <- cpp.GetStateYmuY1( length(y), curState$K, y,
                            curState$z, curState$mu );
  denom <- ssy + curState$sigma2 / curState$hypers$sigma2a;
  normSamples * sqrt(curState$sigma2/denom) + rt/denom;
}

Update.sigma2 <- function( curState, y, nsums ) {
  rt <- cpp.GetStateSquaredResiduals( length(y), curState$K, y,
                                      curState$z, curState$mu,
                                      curState$a );
  a <- curState$hypers$a + nsums/2;
  b <- curState$hypers$b + rt/2;
  b[is.nan(b)] <- curState$hypers$b;
  rinvgamma(curState$K, a, b);  
}

SwitchingARSampler <- function( hypers, K, y, nBurn, nSamp,
                                verbose=FALSE, nAnnounce=500,
                                includeBurnin=FALSE, keepEvery=1 ) {
  vprint <- function(...){;}
  if ( verbose ) {
    vprint <- function(...) {
      print(paste(..., sep=""));
    }
  }
  T <- length(y);
  curState <- InitializeStateSmart( hypers, K, T, y);
  curState <- ComputeLikelihoods( curState, y );
  while( !CanUpdateZ(curState) ) {
    vprint("Can't update Z. Trying again.");
    curState <- InitializeStateSmart( hypers, K, T, y);
    curState <- ComputeLikelihoods( curState, y );
  }
  burnin <- NULL;
  samples <- NULL;
  bestState <- curState;
  bestLik <- -1e100;
  for ( i in 1:(nBurn+nSamp) ) {
    curState <- ComputeLikelihoods( curState, y );
    curState <- Update.z(curState);
    curState <- Update.Pi(curState);
    curState <- Update.theta(curState, y);
    curState$loglik <- StateLogLik(curState, y);
    if ( curState$loglik > bestLik ) {
        bestState <- curState;
        bestLik <- curState$loglik;
    }
    curState$likelihoods <- NULL;
    if ( i <= nBurn ) {
      if ( includeBurnin ) {
        burnin[[i]] <- curState;
      }
    } else if ( i > nBurn && (i-nBurn) %% keepEvery == 0 ) {
      samples[[(i-nBurn)/keepEvery]] <- curState;
    }
    if ( i %% nAnnounce == 0 ) {
      vprint("completed ", i, " iterations");
      if ( verbose ) { timestamp(); }
    }
  }
  list(burnin=burnin, samples=samples, bestSample=bestState);
}

StateLogLik <- function( curState, y ) {
  nsums <- table(c(curState$z, 1:curState$K)) - 1;
  resids <- y - curState$mu[curState$z] -
    curState$a[curState$z] * c(0, y[-length(y)]);
  loglik.y <- -.5 * sum(c(resids^2 / curState$sigma2[curState$z], nsums * log(curState$sigma2)));
  loglik.mu <- -.5 * sum((nsums > 0) * curState$mu^2 / curState$hypers$sigma2mu);
  loglik.a <- -.5 * sum((nsums > 0) * curState$a^2 / curState$hypers$sigma2a);
  loglik.sigma2 <- -(curState$hypers$a-1)*sum((nsums > 0) * log(curState$sigma2)) -
    curState$hypers$b * sum((nsums > 0) * 1/curState$sigma2);
  loglik.z <- sum(sapply(2:length(y), function(t) {
    log(curState$Pi[curState$z[t-1], curState$z[t]]);
  }));
  loglik.y + loglik.mu + loglik.a + loglik.sigma2 + loglik.z;
}

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



