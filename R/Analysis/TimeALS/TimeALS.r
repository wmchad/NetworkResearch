Tmf.UpdateL <- function(L, R, X, lambda) {
  T <- dim(L)[3];
  Lnew <- L
  Lnew[,,1] <- Tmf.UpdateLt(L[,,1], R[,,1], X[,,1], .5*L[,,2], lambda);
  for (t in 2:(T-1)) {
    Lbar <- (L[,,t-1] + L[,,t+1])/2;
    Lnew[,,t] <- Tmf.UpdateLt(L[,,t], R[,,t], X[,,t], Lbar, lambda);
  }
  Lnew[,,T] <- Tmf.UpdateLt(L[,,T], R[,,T],
                            X[,,T], .5*L[,,T-1],
                            lambda);
  Lnew;
}

Tmf.UpdateLt <- function(L, R, X, Lbar, lambda) {
  k <- ncol(L)
  Lnew <- L
  for (u in 1:nrow(L)) {
    nonzero.idx <- which(X[u,] != 0);
    num.nonzero <- length(nonzero.idx);
    if ( num.nonzero >= k ) {
      Xnz <- X[u,nonzero.idx];
      Rnz <- R[nonzero.idx,];
      Lnew[u,] <- solve(crossprod(Rnz, Rnz) + lambda * diag(k)) %*%
        (crossprod(Rnz, Xnz) + lambda * Lbar[u,]);
    }
    else {
      Lnew[u,] <- Lbar[u,];
    }
  }
  Lnew;
}

Tmf.UpdateR <- function(L, R, X, lambda) {
  T <- dim(R)[3];
  Rnew <- R;
  Rnew[,,1] <- Tmf.UpdateRt(L[,,1], R[,,1], X[,,1], .5*R[,,2], lambda);
  for (t in 2:(T-1)) {
    Rbar <- (R[,,t-1] + R[,,t+1])/2;
    Rnew[,,t] <- Tmf.UpdateRt(L[,,t], R[,,t], X[,,t], Rbar, lambda);
  }
  Rnew[,,T] <- Tmf.UpdateRt(L[,,T], R[,,T],
                            X[,,T], .5*R[,,T-1],
                            lambda);
  Rnew;
}

Tmf.UpdateRt <- function(L, R, X, Rbar, lambda) {
  k <- ncol(R);
  Rnew <- R;
  for (v in 1:nrow(R)) {
    nonzero.idx <- which(X[,v] != 0);
    num.nonzero <- length(nonzero.idx);
    if ( length(nonzero.idx) >= k ) {
      Xnz <- X[nonzero.idx,v];
      Lnz <- L[nonzero.idx,];
      Rnew[v,] <- solve(crossprod(Lnz, Lnz) + lambda * diag(k)) %*%
        (crossprod(Lnz, Xnz) + lambda * Rbar[v,]);
    }
    else {
      Rnew[v,] <- Rbar[v,];
    }
  }
  Rnew;
}

Tmf.Objective <- function( L, R, X, lambdaL, lambdaR ) {
  T <- dim(X)[3];
  Xsub <- as.vector(X[,,1]);
  nonzero.idx <- which(Xsub != 0);
  Xsub <- Xsub[nonzero.idx];
  guesses <- as.vector(tcrossprod(L[,,1], R[,,1]))[nonzero.idx];
  obj <- sum((guesses - Xsub)^2) +
    .5*lambdaL * sum(L[,,1]^2) +
      .5*lambdaR * sum(R[,,1]^2);
  for ( t in 2:T ) {
    Xsub <- as.vector(X[,,t]);
    nonzero.idx <- which(Xsub != 0);
    Xsub <- Xsub[nonzero.idx];
    guesses <- as.vector(tcrossprod(L[,,t], R[,,t]))[nonzero.idx];
    obj <- obj + sum((guesses - Xsub)^2) +
      .5*lambdaL * sum((L[,,t]-L[,,t-1])^2) + .5*lambdaR * sum((R[,,t]-R[,,t-1])^2);
  }
  obj;
}

Tmf.RMSE <- function(L, R, X) {
  T <- dim(X)[3];
  f <- 0;
  N <- 0;
  for ( t in 1:T ) {
    Xsub <- as.vector(X[,,t]);
    nonzero.idx <- which(Xsub != 0);
    N <- N + length(nonzero.idx);
    Xsub <- Xsub[nonzero.idx];
    guesses <- as.vector(tcrossprod(L[,,t], R[,,t]))[nonzero.idx];
    f <- f + sum((guesses - Xsub)^2);
  }
  sqrt(f/N);
}

Tmf.ALS <- function(X, ldim, lambda, L0=NULL, R0=NULL,
                maxiter=100, epsilon=1e-6) {
  L <- L0;
  R <- R0;
  if ( is.null(L) ) {
    L <- array(1 + runif(dim(X)[1]*dim(X)[3]*ldim, -.05, .05),
               dim=c(dim(X)[1], ldim, dim(X)[3]));
  }
  if ( is.null(R) ) {
    R <- array(1 + runif(dim(X)[2]*dim(X)[3]*ldim, -.05, .05),
               dim=c(dim(X)[2], ldim, dim(X)[3]));
  }
  rmses <- objvals <- rep(0, maxiter+1);
  objvals[1] <- Tmf.Objective(L, R, X, lambda, lambda);
  rmses[1] <- Tmf.RMSE(L, R, X);
  lastiter <- 0;
  for ( i in 2:(maxiter+1) ) {
    lastiter <- i;
    L <- Tmf.UpdateL(L, R, X, lambda);
    R <- Tmf.UpdateR(L, R, X, lambda);
    objvals[i] <- Tmf.Objective(L, R, X, lambda, lambda);
    rmses[i] <- Tmf.RMSE(L, R, X);
    if ( abs(objvals[i] - objvals[i-1]) < epsilon ) {
      break;
    }
    if ( i %% 50 == 0 ) {
      print(paste("Iteration", i, "finished"));
    }
  }
  list(L=L, R=R, objvals=objvals[1:lastiter], rmses=rmses[1:lastiter],
       lambda=lambda, iter=lastiter-1, epsilon=epsilon);
}

UpdateLwithZeroes <- function(L, R, X, lambda) {
  k <- ncol(L)
  Lnew <- L
  for (u in 1:nrow(L)) {
    Lnew[u,] <- solve(crossprod(R, R) + lambda * diag(k)) %*% crossprod(R, X[u,]);
  }
  Lnew
}

UpdateRwithZeroes <- function(L, R, X, lambda) {
  k <- ncol(R)
  Rnew <- R
  for (v in 1:nrow(R)) {
    Rnew[v,] <- solve(crossprod(L, L) + lambda * diag(k)) %*% crossprod(L, X[,v])
  }
  Rnew
}

fLRwithZeroes <- function( L, R, Xmat, lambdaL, lambdaR ) {
  X <- as.vector(Xmat)
  guesses <- as.vector(tcrossprod(L, R))
  0.5 * (sum((guesses - X)^2) + lambdaL * sum(L^2) + lambdaR * sum(R^2))
}

RMSEwithZeroes <- function(L, R, Xmat) {
  X <- as.vector(Xmat)
  guesses <- as.vector(tcrossprod(L, R))
  sqrt(mean((guesses - X)^2))
}

ALSwithZeroes <- function(X, ldim, lambda, L0=NULL, R0=NULL,
                          maxiter=100, epsilon=1e-6) {
  L <- L0;
  R <- R0;
  if ( is.null(L) ) {
    L <- matrix(1 + runif(nrow(X)*ldim, -.05, .05),
                nrow=nrow(X), ncol=ldim);
  }
  if ( is.null(R) ) {
    R <- matrix(1 + runif(ncol(X)*ldim, -.05, .05),
                nrow=ncol(X), ncol=ldim);
  }
  rmses <- fvals <- rep(0, maxiter+1);
  fvals[1] <- fLRwithZeroes(L, R, X, lambda, lambda);
  rmses[1] <- RMSEwithZeroes(L, R, X);
  lastiter <- 0;
  for ( i in 2:(maxiter+1) ) {
    lastiter <- i;
    L <- UpdateLwithZeroes(L, R, X, lambda);
    R <- UpdateRwithZeroes(L, R, X, lambda);
    fvals[i] <- fLRwithZeroes(L, R, X, lambda, lambda);
    rmses[i] <- RMSEwithZeroes(L, R, X);
    if ( abs(fvals[i] - fvals[i-1]) < epsilon ) {
      break;
    }
  }
  list(L=L, R=R, fvals=fvals[1:lastiter], rmses=rmses[1:lastiter],
       lambda=lambda, iter=lastiter-1, epsilon=epsilon);
}

