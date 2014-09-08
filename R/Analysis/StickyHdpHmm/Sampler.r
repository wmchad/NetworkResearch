require(MCMCpack);

source("c:/Code/NetworkResearch/R/Analysis/StickyHdpHmm/SamplerRcpp.r");

## 2/24/2014
## Block Gibbs sampler for sticky HDP-HMM


##################################################
## Algorithm
##################################################

## Data: y_1, ..., y_T
## Hidden States: z_{1:T} in {1, ..., L}
## Each state k has parameters
##   pi_k: transition probabilities from state k
##   mu_k: state mean impulse
##   a_k: state auto-regression parameter
##   sigma^2: state innovation variance
## n_jk = number of transitions from state j to state k

## Initialize parameters:
##   L = ??? (some highish number)
##   (alpha + kappa) ~ Gamma(a_1, b_1)
##   gamma ~ Gamma(a_2, b_2)
##   rho = [kappa / (alpha + kappa)] ~ Beta(c, d)
##   nu_k ~ Beta(1, gamma), k = 1, ..., L
##   beta_k = nu_k * \Prod_{i=1}^{k-1} (1 - nu_i)
##   pi_k ~ DP(alpha + kappa, (alpha * beta + kappa * delta_k)/(alpha + kappa)), k = 1, ..., L
##   mu_k ~ N(0, sigma^2_mu), k = 1, ..., L
##   a_k ~ N(0, sigma^2_a), k = 1, ..., L
##   sigma^2_k ~ IG(a_3, b_3), k = 1, ..., L

## Block sample z_{1:T} using backwards-forwards algorithm

## Update beta
##   Sample m_jk for (j,k) in {1, ..., L}^2
##     set m_jk = 0, n=0
##     for i = 1, ..., n_jk
##       sample x ~ Bernoulli((alpha * beta_k + kappa * delta(j,k)) /
##                            (n + alpha * beta_k + kappa * delta(j,k)))
##       increment n
##       if x = 1, increment m_jk
##   Sample w_jdot for j = 1, ..., L
##     sample w_jdot ~ Binomial( m_jj, rho / (rho + beta_j * (1 - rho)) )
##   Calculate m_jk bar
##     m_jk bar = m_jk if j != k
##     m_jk bar = m_jk - w_jdot if j = k
##   Sample beta ~ Dirichlet(gamma/L + m_dot1 bar, ..., gamma/L + m_dotL bar)

## Update pi for k = 1, ..., L
##   pi_k ~ Dirichlet( alpha * beta_1 + n_k1, ..., alpha * beta_k + kappa + n_kk, ...,
##                     alpha * beta_L + n_kL )

## Update a, mu, sigma^2

## Update alpha, gamma, kappa
##   Sample (alpha + kappa)
##     sample r_j ~ Beta( alpha + kappa + 1, n_jdot )
##     sample s_j ~ Bernoulli( n_jdot / (n_jdot + alpha + kappa)
##     sample (alpha + kappa) ~ Gamma ( a1 + m_dotdot - sum_j(s_j), b_1 - sum_j(log r_j) )
##   Sample gamma
##     sample eta ~ Beta(gamma + 1, m_dotdot bar)
##     Kbar = sum_k(Indicator(m_dotk bar > 0))
##     sample gamma ~ pi_mbar * Gamma(a_2 + Kbar, b_2 - log eta) +
##                    (1-pi_mbar) * Gamma(a_2 + Kbar - 1, b_2 - log eta)
##   Sample rho ~ Beta(sum_j(w_jdot) + c, m_dotdot - sum_j(w_jdot) + d)

InitializeState <- function( hypers, L, T ) {
  sampler = list(hypers=hypers, L=L, T=T);
  apk <- rgamma(1, hypers$a1, hypers$b1);
  rho <- rbeta(1, hypers$c, hypers$d);
  sampler$kappa <- rho * apk;
  sampler$alpha <- apk - sampler$kappa
  sampler$gamma <-  rgamma(1, hypers$a2, hypers$b2);
  sampler$nus <- rbeta(L, 1, sampler$gamma);
  sampler$beta <- rep(0, L);
  sampler$beta[1] <- sampler$nus[1];
  for ( i in 2:L ) {
    sampler$beta[i] <- (1 - sum(sampler$beta[1:(i-1)])) * sampler$nus[i];
  }
  sampler$Pi0 <- matrix(nrow=L, ncol=L);
  for ( i in 1:L ) {
    delta <- rep(0, L);
    delta[i] <- 1;
    sampler$Pi[i,] <- rdirichlet(1, sampler$alpha * sampler$beta + delta * sampler$kappa);
  }
  sampler$mu <- rnorm(L, 0, hypers$sigma2mu);
  sampler$a <- rnorm(L, 0, hypers$sigma2a);
  sampler$sigma2 <- rinvgamma(L, hypers$a3, hypers$b3);
  sampler;
}

ComputeLikelihoods <- function( curState, y ) {
  lkl <- cpp.GetLikelihoods(length(y), curState$L,
                             y, curState$mu,
                             curState$a, curState$sigma2);
  lkl <- lkl - max( -300, mean(lkl));
  curState$likelihoods <- exp(lkl);
##   lkl <- matrix(nrow=curState$T, ncol=curState$L);
##   lkl[1,] <- -.5*log(curState$sigma2) - (.5*(y[1] - curState$mu)^2)/curState$sigma2;
##   lkl[-1,] <- t(sapply(2:curState$T, function(t) {
##     -.5*log(curState$sigma2) - (.5*(y[t] - curState$mu - curState$a * y[t-1])^2)/curState$sigma2;
##   }));
  curState$likelihoods <- exp(lkl);
  curState;
}

CanUpdateZ <- function( curState ) {
  fwdProb <- cpp.GetForwardProbabilities(curState$T, curState$L,
                                         curState$Pi, curState$likelihoods)[-1,];
  sum(is.nan(fwdProb)) == 0;
}

Update.z <- function( curState ) {
  fwdProb <- cpp.GetForwardProbabilities(curState$T, curState$L,
                                         curState$Pi, curState$likelihoods)[-1,];
  n <- matrix(nrow=curState$L, ncol=curState$L, data=0);
  z <- rep(0, curState$T);
  if (sum(fwdProb[1,]) == 0) {
    print("sampling z[1] uniformly");
    z[1] <- sample(curState$L, 1);    
  } else {
    z[1] <- sample(curState$L, 1, prob=fwdProb[1,]);
  }
  for ( t in 2:curState$T ) {
    if (sum(curState$Pi[z[t-1],] * fwdProb[t,]) == 0) {
      print("sampling z uniformly");
      z[t] <- sample(curState$L, 1);    
    }
    else {
      z[t] <- sample(curState$L, 1, prob=curState$Pi[z[t-1],] * fwdProb[t,]);
    }
    n[z[t-1],z[t]] <- n[z[t-1],z[t]] + 1;
  }
  curState$z <- z;
  curState$n <- n;
  curState;
}

Update.beta <- function( curState ) {
  m <- matrix(nrow=curState$L, ncol=curState$L);
  for ( j in 1:curState$L ) {
    for ( k in 1:curState$L ) {
      mjk <- 0;
      n <- 0;
      for ( i in 1:curState$n[j,k] ) {
        mjk <- mjk + rbinom(1, 1, (curState$alpha * curState$beta[k] + curState$kappa * (j==k)) /
                            (n + curState$alpha * curState$beta[k] + curState$kappa * (j==k)));
        n <- n+1;
      }
      m[j,k] <- mjk;
    }
  }
  mbar <- m;
  w <- rep(0, curState$L);
  for ( j in 1:curState$L ) {
    w[j] <- rbinom(1, m[j,j], 1 / (1 + curState$beta[j] * curState$alpha / curState$kappa));
    mbar[j,j] <- mbar[j,j] - w[j];
  }
  curState$beta <- rdirichlet(1, curState$gamma / curState$L + apply(mbar, 2, sum));
  curState$m <- m;
  curState$mbar <- mbar;
  curState$w <- w;
  curState;
}

Update.Pi <- function( curState ) {
  PiNext <- matrix(nrow=curState$L, ncol=curState$L);
  for ( i in 1:curState$L ) {
    delta <- rep(0, curState$L);
    delta[i] <- 1;
    PiNext[i,] <- rdirichlet(1, curState$alpha * curState$beta +
                             delta * curState$kappa + curState$n[i,]);
  }
  curState$Pi <- PiNext;
  curState;
}

Update.theta <- function( curState, y ) {
  ## Prep
  nsums <- table(c(curState$z, 1:curState$L)) - 1;
  nThetaSamples <- 50;
  normSamples <- rnorm(2 * curState$L * nThetaSamples, 0, 1);
  startRand <- 0;
  ssy <- cpp.GetStateSSYs( length(y), curState$L, y, curState$z);
  ## mu update
  for ( i in 1:nThetaSamples ) {
    curState$mu <- Update.mu( curState, y, nsums, normSamples[(1:curState$L) + startRand] );
    startRand <- startRand + curState$L;
    curState$sigma2 <- Update.sigma2( curState, y, nsums );
    curState$a <- Update.a( curState, y, normSamples[(1:curState$L) + startRand], ssy );
    startRand <- startRand + curState$L;
  }
  curState;
}

Update.mu <- function( curState, y, nsums, normSamples ) {
  rt <- cpp.GetStateArResids( length(y), curState$L, y,
                               curState$z, curState$a );
  normSamples * sqrt(curState$sigma2 / (nsums + curState$sigma2 / curState$hypers$sigma2mu)) + 
        rt / (nsums + curState$sigma2 / curState$hypers$sigma2mu);
}

Update.a <- function( curState, y, normSamples, ssy ) {
  rt <- cpp.GetStateYmuY1( length(y), curState$L, y,
                            curState$z, curState$mu );
  denom <- ssy + curState$sigma2 / curState$hypers$sigma2a;
  normSamples * sqrt(curState$sigma2/denom) + rt/denom;
}

Update.sigma2 <- function( curState, y, nsums ) {
  rt <- cpp.GetStateSquaredResiduals( length(y), curState$L, y,
                                      curState$z, curState$mu,
                                      curState$a );
  a <- curState$hypers$a3 + nsums/2;
  b <- curState$hypers$b3 + rt/2;
  b[is.nan(b)] <- curState$hypers$b3;
  rinvgamma(curState$L, a, b);  
}

Update.Hypers <- function( curState ) {
  nrowsums <- apply(curState$n, 1, sum);
  mdotdot <- sum(curState$m);
  mdotdotbar <- sum(curState$mbar);
  mbarcolsums <- apply(curState$mbar, 2, sum);
  wdotdot <- sum(curState$w);
  r <- rbeta(sum(nrowsums > 0), curState$alpha + curState$kappa + 1, nrowsums[nrowsums > 0]);
  s <- rbinom(curState$L, 1, nrowsums / (nrowsums + curState$alpha + curState$kappa));
  aplusk <- rgamma(1, curState$hypers$a1 + mdotdot - sum(s),
                   curState$hypers$b1 - sum(log(r)));
  rho <- rbeta(1, wdotdot + curState$hypers$c, mdotdot - wdotdot + curState$hypers$d);
  curState$alpha <- (1-rho)*aplusk;
  curState$kappa <- rho*aplusk;
  eta <- rbeta(1, curState$gamma + 1, mdotdotbar);
  kbar <- sum(mbarcolsums > 0);
  pimbar <- (curState$hypers$a2 + kbar - 1) / (mdotdotbar * (curState$hypers$b2 - log(eta)));
  if ( runif(1,0,1) <= pimbar ) {
    curState$gamma <- rgamma(1, curState$hypers$a2 + kbar,
                    curState$hypers$b2 - log(eta));
  } else {
    curState$gamma <- rgamma(1, curState$hypers$a2 + kbar - 1,
                    curState$hypers$b2 - log(eta));
  }
  curState;
}

HdpHmmSampler <- function( hypers, L, y, nBurn, nSamp,
                           verbose=FALSE, nAnnounce=500,
                           includeBurnin=FALSE, keepEvery=1  ) {
  vprint <- function(...){;}
  if ( verbose ) {
    vprint <- function(...) {
      print(paste(..., sep=""));
    }
  }
  T <- length(y);
  curState <- InitializeState( hypers, L, T);
  curState <- ComputeLikelihoods( curState, y );
  while( !CanUpdateZ(curState) ) {
    vprint("Can't update Z. Trying again.");
    curState <- InitializeState( hypers, L, T);
    curState <- ComputeLikelihoods( curState, y );
  }
  burnin <- NULL;
  samples <- NULL;
  bestState <- curState;
  bestLik <- -1e100;
  for ( i in 1:(nBurn+nSamp) ) {
    curState <- ComputeLikelihoods( curState, y );
    curState <- Update.z(curState);
    curState <- Update.beta(curState);
    curState <- Update.Pi(curState);
    curState <- Update.Hypers(curState);
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
  nsums <- table(c(curState$z, 1:curState$L)) - 1;
  resids <- y - curState$mu[curState$z] -
    curState$a[curState$z] * c(0, y[-length(y)]);
  loglik.y <- -.5 * sum(c(resids^2 / curState$sigma2[curState$z], nsums * log(curState$sigma2)));
  loglik.mu <- -.5 * sum((nsums > 0) * curState$mu^2 / curState$hypers$sigma2mu);
  loglik.a <- -.5 * sum((nsums > 0) * curState$a^2 / curState$hypers$sigma2a);
  loglik.z <- sum(sapply(2:length(y), function(t) {
    log(curState$Pi[curState$z[t-1], curState$z[t]]);
  }));
  loglik.y + loglik.mu + loglik.a + loglik.z;
}

