SymmetricMF.GetCost <- function(Xarray, ldim, lambda, t1, t2,
                                maxiter=100, epsilon=1e-2, verbose=FALSE) {
  subArray <- Xarray[,,t1:t2];
  if ( t1==t2 ) {
    subArray <- array(dim=c(dim(Xarray)[1:2], 1), data=Xarray[,,t1]);
  }
  U0 <- NULL;
  mu <- mean(Xarray);
  sigma <- sd(Xarray);
  n <- dim(subArray)[1];
  U0 <- matrix(nrow=n, ncol=ldim, data=rnorm(n*ldim, mu, sigma/ldim));
  result <- SymmetricMF(subArray, ldim, lambda, U0, maxiter,
                        epsilon, verbose=verbose);
  list(t1=t1, t2=t2, cost=result$objvals[result$iter+1],
       rmse=result$rmses[result$iter+1], U=result$U);
}

SymmetricMF.Changepoint.PELT <- function(Xarray, ldim, lambda, beta, maxiter=100,
                                         epsilonInter=1e-2, epsilonFinal=1e-3,
                                         verbose=FALSE) {
  
  n <- dim(Xarray)[3];
  F <- rep(Inf, n+1);
  F[1] <- -beta;
  cp <- matrix(nrow=n+1, ncol=n, data=FALSE);
  R <- NULL;
  R[[1]] <- c(-1,0);
  K <- 0;
  costs <- NULL;

  for ( t in 1:n ) {
    tbest <- NULL;
    Fvals <- NULL;
    for( t1 in R[[t]][-1] ) {
      costRef <- paste((t1+1), t, sep="-");
      if ( is.null(costs[[costRef]]) ) {
        costs[[costRef]] <- SymmetricMF.GetCost(Xarray, ldim, lambda,
                                                t1+1, t, maxiter,
                                                epsilonInter, verbose);
      }
      Fval <- F[t1+1] + costs[[costRef]]$cost;
      Fvals <- c(Fvals, Fval);
      if ( Fval + beta < F[t+1] ) {
        F[t+1] <- Fval + beta;
        tbest <- t1;
      }
    }
    Rcur <- c(-1,t);
    cp[t+1,] <- cp[tbest+1,];
    cp[t+1,tbest] <- TRUE;
    for ( i in 2:length(R[[t]]) ) {
      if ( Fvals[i-1] + K < F[t+1] ) {
        Rcur <- c(Rcur, R[[t]][i]);
      }
    }
    R[[t+1]] <- Rcur;
  }
  cps <- (1:n)[cp[n+1,]];
  segments <- NULL;
  tstart <- c(0, cps);
  tend <- c(cps, n);
  for ( i in 1:length(tstart) ) {
    segRef <- paste((tstart[i]+1), tend[i], sep="-");
    segments[[segRef]] <- costs[[segRef]];
  }
  list(cp=cps, segments=segments);
}

SymmetricMF.Changepoint.OP <- function(Xarray, ldim, beta, maxiter=100,
                                    epsilonInter=1e-2, epsilonFinal=1e-3) {
  
  n <- dim(Xarray)[3];
  F <- rep(Inf, n+1);
  F[1] <- -beta;
  cp <- matrix(nrow=n+1, ncol=n+1, data=FALSE);
  R <- NULL;
  R[[1]] <- c(-1,0);
  K <- 0;
  costs <- NULL;

  for ( t in 1:n ) {
    tbest <- NULL;
    for( t1 in 0:(t-1) ) {
      costRef <- paste((t1+1), t, sep="-");
      if ( is.null(costs[[costRef]]) ) {
        costs[[costRef]] <- SymmetricMF.GetCost(Xarray, ldim, 0,
                                                t1+1, t, maxiter,
                                                epsilonInter);
      }
      Fval <- F[t1+1] + costs[[costRef]]$cost + beta;
      if ( Fval < F[t+1] ) {
        F[t+1] <- Fval;
        tbest <- t1;
      }
    }
    cp[t+1,] <- cp[tbest+1,];
    cp[t+1,tbest] <- TRUE;
  }
  cps <- (1:n)[cp[n+1,]];
  segments <- NULL;
  tstart <- c(0, cps);
  tend <- c(cps, n);
  for ( i in 1:length(tstart) ) {
    segRef <- paste((tstart[i]+1), tend[i], sep="-");
    segments[[segRef]] <- costs[[segRef]];
  }
  list(cp=cps, segments=segments);
}
