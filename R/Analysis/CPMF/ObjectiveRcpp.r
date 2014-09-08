require(Rcpp);
require(RcppArmadillo);
require(inline);

src.SymmetricMF.Objective <- '
  NumericMatrix xHat(Xhat);
  NumericVector X(x);
  const double penalty = as<double>(pen);

  IntegerVector arrayDims = X.attr("dim");
  arma::cube data(X.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);

  double obj = 0;

  for ( int i = 0; i < arrayDims[0]; i++ ) {
    for ( int j = 0; j < arrayDims[1]; j++ ) {
      for ( int t = 0; t < arrayDims[2]; t++ ) {
        if ( data(i, j, t) != 0 ) {
          obj += pow(data(i, j, t) - xHat(i, j), 2);
        }
      }
    }
  }
  return wrap(0.5 * obj + penalty);
';

cpp.SymmetricMF.Objective <-
  cxxfunction(signature(Xhat="numeric",
                        x="numeric",
                        pen="numeric"),
              src.SymmetricMF.Objective, plugin="RcppArmadillo");
