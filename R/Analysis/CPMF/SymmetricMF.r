SymmetricMF.UpdateU <- function(U, Xarray, lambda) {
  k <- ncol(U)
  Unew <- U
##  lastRmse <- curRmse <- cpp.SymmetricMF.RMSE(tcrossprod(Unew, Unew), Xarray);
  for (u in 1:nrow(U)) {
    Uold <- Unew[u,];
    num.nonzero <- 0;
    Xnz <- NULL;
    Unz <- NULL;
    for ( t in 1:dim(Xarray)[3] ) {
      nonzero.idx <- which(Xarray[u,,t] != 0);
      num.nonzero <- num.nonzero + length(nonzero.idx);
      Xnz <- c(Xnz, Xarray[u,nonzero.idx,t]);
      Unz <- rbind(Unz, Unew[nonzero.idx,]);
    }
    if ( num.nonzero >= k ) {
      Unew[u,] <- solve(crossprod(Unz, Unz) + lambda * diag(k)) %*% crossprod(Unz, Xnz);
##      curRmse <- cpp.SymmetricMF.RMSE(tcrossprod(Unew, Unew), Xarray);
##      if ( curRmse > lastRmse ) {
##        Unew[u,] <- Uold;
##      }
##      else {
##        lastRmse <- curRmse;
##      }
    }
  }
  Unew;
}

SymmetricMF <- function( Xarray, ldim, lambda, U0=NULL,
                         maxiter=100, epsilon=1e-3, verbose=FALSE ) {
  vprint <- function(...){};
  if ( verbose ) {
    vprint <- function(...){print(paste(...));};
  }
  N <- dim(Xarray)[1];
  U <- U0;
  if ( is.null(U) ) {
    U <- matrix(1 + runif(N*ldim, -.05, .05),
                nrow=N, ncol=ldim);
  }
  rmses <- objvals <- rep(0, maxiter+1);
  Xhat <- tcrossprod(U, U);
  objvals[1] <- cpp.SymmetricMF.Objective(Xhat, Xarray, lambda * sum(U^2));
  rmses[1] <- cpp.SymmetricMF.RMSE(Xhat, Xarray);
  lastiter <- 0;
  for ( i in 2:(maxiter+1) ) {
    lastiter <- i;
    vprint("    iteration", i-1);
    Unew <- cpp.SymmetricMF.UpdateU(U, Xarray, lambda, 0);
    vprint("      Unew$obj =", Unew$obj);
    vprint("      objvals[i-1] =", objvals[i-1]);
    if ( Unew$obj > objvals[i-1] ) {
      Unew <- cpp.SymmetricMF.UpdateU(U, Xarray, lambda, 1);
    }
    vprint("    iteration", i-1, "finished");
    U <- Unew$U;
    objvals[i] <- Unew$obj;
    rmses[i] <- Unew$rmse;
    vprint("    objv:", objvals[i]);
    vprint("    RMSE:", rmses[i]);
    if ( abs(objvals[i] - objvals[i-1])/objvals[i-1] < epsilon ) {
      break;
    }
  }
  list(U=U, objvals=objvals[1:lastiter], rmses=rmses[1:lastiter],
       lambda=lambda, iter=lastiter-1, epsilon=epsilon);  
}

MF <- function( Xarray, ldim, lambda, U0=NULL,
                         maxiter=100, epsilon=1e-3, verbose=FALSE ) {
  vprint <- function(...){};
  if ( verbose ) {
    vprint <- function(...){print(paste(...));};
  }
  N <- dim(Xarray)[1];
  U <- U0;
  if ( is.null(U) ) {
    U <- matrix(1 + runif(N*ldim, -.05, .05),
                nrow=N, ncol=ldim);
  }
  rmses <- objvals <- rep(0, maxiter+1);
  Xhat <- tcrossprod(U, U);
  objvals[1] <- cpp.SymmetricMF.Objective(Xhat, Xarray, lambda * sum(U^2));
  rmses[1] <- cpp.SymmetricMF.RMSE(Xhat, Xarray);
  lastiter <- 0;
  for ( i in 2:(maxiter+1) ) {
    lastiter <- i;
    vprint("    iteration", i-1);
    Unew <- cpp.SymmetricMF.UpdateU(U, Xarray, lambda, 0);
    vprint("      Unew$obj =", Unew$obj);
    vprint("      objvals[i-1] =", objvals[i-1]);
    if ( Unew$obj > objvals[i-1] ) {
      Unew <- cpp.SymmetricMF.UpdateU(U, Xarray, lambda, 1);
    }
    vprint("    iteration", i-1, "finished");
    U <- Unew$U;
    objvals[i] <- Unew$obj;
    rmses[i] <- Unew$rmse;
    vprint("    objv:", objvals[i]);
    vprint("    RMSE:", rmses[i]);
    if ( abs(objvals[i] - objvals[i-1])/objvals[i-1] < epsilon ) {
      break;
    }
  }
  list(U=U, objvals=objvals[1:lastiter], rmses=rmses[1:lastiter],
       lambda=lambda, iter=lastiter-1, epsilon=epsilon);  
}
