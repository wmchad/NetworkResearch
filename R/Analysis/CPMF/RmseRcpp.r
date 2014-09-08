require(Rcpp);
require(RcppArmadillo);
require(inline);

src.SymmetricMF.RMSE <- '
  NumericMatrix xHat(Xhat);
  NumericVector X(x);
  IntegerVector arrayDims = X.attr("dim");

  arma::cube data(X.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);

  int n = 0;
  double sse = 0;

  for ( int i = 0; i < arrayDims[0]; i++ ) {
    for ( int j = 0; j < arrayDims[1]; j++ ) {
      for ( int t = 0; t < arrayDims[2]; t++ ) {
        if ( data(i, j, t) != 0 ) {
          n++;
          sse += pow(data(i, j, t) - xHat(i, j), 2);
        }
      }
    }
  }
  return wrap(pow(sse / n, 0.5));
';

cpp.SymmetricMF.RMSE <-
  cxxfunction(signature(Xhat="numeric",
                        x="numeric"),
              src.SymmetricMF.RMSE, plugin="RcppArmadillo");
