UpdateL <- function(L, R, X, lambda) {
  k <- ncol(L)
  Lnew <- L
  for (u in 1:nrow(L)) {
    nonzero.idx <- which(X[u,] != 0)
    num.nonzero <- length(nonzero.idx)
    Xnz <- X[u,nonzero.idx]
    Rnz <- R[nonzero.idx,]
    if ( length(nonzero.idx) >= k ) {
      Lnew[u,] <- solve(crossprod(Rnz, Rnz) + lambda * diag(k)) %*% crossprod(Rnz, Xnz);
    }
  }
  Lnew;
}

UpdateR <- function(L, R, X, lambda) {
  k <- ncol(R)
  Rnew <- R
  for (v in 1:nrow(R)) {
    nonzero.idx <- which(X[,v] != 0)
    num.nonzero <- length(nonzero.idx)
    Xnz <- X[nonzero.idx,v]
    Lnz <- L[nonzero.idx,]
    if ( length(nonzero.idx) >= k ) {
      Rnew[v,] <- solve(crossprod(Lnz, Lnz) + lambda * diag(k)) %*% crossprod(Lnz, Xnz)
    }
  }
  Rnew;
}

fLR <- function( L, R, Xmat, lambdaL, lambdaR ) {
  X <- as.vector(Xmat)
  nonzero.idx <- which(X != 0)
  X <- X[nonzero.idx]
  guesses <- as.vector(tcrossprod(L, R))[nonzero.idx]
  0.5 * (sum((guesses - X)^2) + lambdaL * sum(L^2) + lambdaR * sum(R^2))
}

RMSE <- function(L, R, Xmat) {
  X <- as.vector(Xmat)
  nonzero.idx <- which(X != 0)
  X <- X[nonzero.idx]
  guesses <- as.vector(tcrossprod(L, R))[nonzero.idx]
  sqrt(mean((guesses - X)^2))
}

ALS <- function(X, ldim, lambda, L0=NULL, R0=NULL,
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
  fvals[1] <- fLR(L, R, X, lambda, lambda);
  rmses[1] <- RMSE(L, R, X);
  lastiter <- 0;
  for ( i in 2:(maxiter+1) ) {
    lastiter <- i;
    L <- UpdateL(L, R, X, lambda);
    R <- UpdateR(L, R, X, lambda);
    fvals[i] <- fLR(L, R, X, lambda, lambda);
    rmses[i] <- RMSE(L, R, X);
    if ( abs(fvals[i] - fvals[i-1]) < epsilon ) {
      break;
    }
  }
  list(L=L, R=R, fvals=fvals[1:lastiter], rmses=rmses[1:lastiter],
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

