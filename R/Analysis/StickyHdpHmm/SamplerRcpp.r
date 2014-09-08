require(Rcpp);
require(inline);

##   rttmp <- aggregate(c((y[1] - curState$mu[curState$z[1]])^2,
##                        (y[-1] - curState$mu[curState$z[-1]] -
##                         curState$a[curState$z[-1]] * y[-length(y)])^2),
##                      list(curState$z), FUN=sum);

src.GetStateSquaredResiduals <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericVector ys(y);
  IntegerVector states(z);
  NumericVector mus(mu);
  NumericVector ars(ar);

  NumericVector resids(k);

  int i;

  for ( i = 0; i < k; i++ ) {
    resids(i) = 0;
  }

  resids(states(0)-1) = pow(ys(0) - mus(states(0)-1), 2);

  for ( i = 1; i < t; i++ ) {
    resids(states(i)-1) += pow(ys(i) - mus(states(i)-1) - ars(states(i)-1) * ys(i-1), 2);
  }

  return wrap(resids);
';

cpp.GetStateSquaredResiduals <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        y="numeric",
                        z="numeric",
                        mu="numeric",
                        ar="numeric"),
              src.GetStateSquaredResiduals, plugin="Rcpp");


##   rt <- rep(0, curState$K);
##   rttmp <- aggregate(c(0, (y[-1] - curState$mu[curState$z[-1]]) * y[-length(y)], rep(0, curState$K)),
##                      list(c(curState$z,1:curState$K)), FUN=sum);
##   rt[rttmp[,1]] <- rttmp[,2];

src.GetStateYmuY1 <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericVector ys(y);
  IntegerVector states(z);
  NumericVector mus(mu);

  NumericVector resids(k);

  int i;

  for ( i = 0; i < k; i++ ) {
    resids(i) = 0;
  }

  for ( i = 1; i < t; i++ ) {
    resids(states(i)-1) += (ys(i) - mus(states(i)-1)) * ys(i-1);
  }

  return wrap(resids);
';

cpp.GetStateYmuY1 <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        y="numeric",
                        z="numeric",
                        mu="numeric"),
              src.GetStateYmuY1, plugin="Rcpp");

##   rt <- rep(0, curState$K);
##   rttmp <- aggregate(c(y[1], y[-1] - curState$a[curState$z[-1]] * y[-length(y)]),
##                      list(curState$z), FUN=sum);
##   rt[rttmp[,1]] <- rttmp[,2];

src.GetStateArResids <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericVector ys(y);
  IntegerVector states(z);
  NumericVector ars(ar);

  NumericVector resids(k);

  int i;

  for ( i = 0; i < k; i++ ) {
    resids(i) = 0;
  }

  resids(states(0)-1) = ys(0);

  for ( i = 1; i < t; i++ ) {
    resids(states(i)-1) += ys(i) - ars(states(i)-1) * ys(i-1);
  }

  return wrap(resids);
';

cpp.GetStateArResids <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        y="numeric",
                        z="numeric",
                        ar="numeric"),
              src.GetStateArResids, plugin="Rcpp");

##   ssy <- rep(0, curState$K);
##   ssytmp <- aggregate(c(0, y[-length(y)]^2,rep(0, curState$K)),
##                         list(c(curState$z,1:curState$K)), FUN=sum);
##   ssy[ssytmp[,1]] <- ssytmp[,2];

src.GetStateSSYs <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericVector ys(y);
  IntegerVector states(z);

  NumericVector ssys(k);

  int i;

  for ( i = 0; i < k; i++ ) {
    ssys(i) = 0;
  }

  for ( i = 1; i < t; i++ ) {
    ssys(states(i)-1) += pow(ys(i-1), 2);
  }

  return wrap(ssys);
';

cpp.GetStateSSYs <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        y="numeric",
                        z="numeric"),
              src.GetStateSSYs, plugin="Rcpp");

##   lkl <- matrix(nrow=curState$T, ncol=curState$K);
##   lkl[1,] <- -.5*log(2*pi*curState$sigma2) - (.5*(y[1] - curState$mu)^2)/curState$sigma2;
##   lkl[-1,] <- t(sapply(2:curState$T, function(t) {
##     -.5*log(2*pi*curState$sigma2) - (.5*(y[t] - curState$mu - curState$a * y[t-1])^2)/curState$sigma2;
##     ## lkls - mean(lkls);
##   }));

src.GetLikelihoods <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericVector ys(y);
  NumericVector mus(mu);
  NumericVector ars(ar);
  NumericVector sigma2s(sigma2);

  NumericMatrix lkls(t, k);

  int z;

  for ( z = 0; z < k; z++ ) {
    lkls(0, z) = -.5 * log(2*M_PI*sigma2s(z)) - .5 * pow(ys(0)-mus(z), 2) / sigma2s(z);
  }

  for ( int i = 1; i < t; i++ ) {
    for ( z = 0; z < k; z++ ) {
      lkls(i, z) = -.5 * log(2*M_PI*sigma2s(z)) - .5 * pow(ys(i)-mus(z)-ars(z)*ys(i-1) , 2) / sigma2s(z);
    }
  }

  return wrap(lkls);
';

cpp.GetLikelihoods <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        y="numeric",
                        mu="numeric",
                        ar="numeric",
                        sigma2="numeric"),
              src.GetLikelihoods, plugin="Rcpp");

##   msgback <- matrix(nrow=curState$T+1, ncol=curState$K);
##   msgback[curState$T+1,] <- 1;
##   for ( t in curState$T:1 ) {
##     msgback[t,] <- sapply(1:curState$K, function(k) {
##       sum(curState$Pi[k,] * curState$likelihoods[t,] * msgback[t+1,]);
##     });
##     msgback[t,] <- msgback[t,] / sum(msgback[t,]);
##   }
##   adjLik <- curState$likelihoods
##   fwdProb <- curState$likelihoods * msgback[-1,];

src.GetForwardProbabilities <- '
  int t = as<int>(T);
  int k = as<int>(K);
  NumericMatrix tpm(Pi);
  NumericMatrix lkls(lkl);

  NumericMatrix msgBack(t+1, k);

  int z, j;

  for ( z = 0; z < k; z++ ) {
    msgBack(t,z) = 1;
  }

  double backSum;
  double curMsg;

  for ( int i = t-1; i >= 0; i-- ) {
    backSum = 0;
    for ( z = 0; z < k; z++ ) {
      msgBack(i,z) = 0;
      for ( j = 0; j < k; j++ ) {
        curMsg = tpm(z,j) * lkls(i,j) * msgBack(i+1,j);
        backSum += curMsg;
        msgBack(i,z) += curMsg;
      }
    }
    for ( z = 0; z < k; z++ ) {
      msgBack(i,z) = msgBack(i,z) / backSum;
      msgBack(i+1,z) = msgBack(i+1,z) * lkls(i,z);
    }

  }

  return wrap(msgBack);
';

cpp.GetForwardProbabilities <-
  cxxfunction(signature(T="numeric",
                        K="numeric",
                        Pi="numeric",
                        lkl="numeric"),
              src.GetForwardProbabilities, plugin="Rcpp");
