require(Rcpp);
require(RcppArmadillo);
require(RcppEigen);
require(inline);

src.SymmetricMF.Objective <- '
using Eigen::MatrixXd;
const double SymObjective(MatrixXd U, arma::cube data, double lambda) {
  double obj = 0;
  double mult = 1;
  double guess = 0;
  const int n = U.rows();
  const int k = U.cols();

  double penalty = 0;
  for ( int i = 0; i < n; i++ ) {
    for ( int j = 0; j < k; j++ ) {
      penalty += pow(U(i,j), 2);
    }
  }
  penalty = penalty * lambda;

  for ( unsigned int i = 0; i < data.n_rows; i++ ) {
    for ( unsigned int j = i; j < data.n_cols; j++ ) {
      guess = 0;
      mult = (i==j) ? 1 : 2;
      for ( unsigned int t = 0; t < data.n_slices; t++ ) {
        if ( data(i, j, t) != 0 ) {
          if ( guess == 0 ) {
            guess = U.row(i) * U.row(j).transpose();
          }
          obj += mult * pow(data(i, j, t) - guess, 2);
        }
      }
    }
  }
  return 0.5 * obj + penalty;
}
';

src.SymmetricMF.RMSE <- '
using Eigen::MatrixXd;
const double SymRmse(MatrixXd U, arma::cube data) {
  int n = 0;
  double sse = 0;
  double mult = 1;
  double guess = 0;

  for ( unsigned int i = 0; i < data.n_rows; i++ ) {
    for ( unsigned int j = i; j < data.n_cols; j++ ) {
      guess = 0;
      mult = (i==j) ? 1 : 2;
      for ( unsigned int t = 0; t < data.n_slices; t++ ) {
        if ( data(i, j, t) != 0 ) {
          if ( guess == 0 ) {
            guess = U.row(i) * U.row(j).transpose();
          }
          n+= mult;
          sse += mult * pow(data(i, j, t) - guess, 2);
        }
      }
    }
  }
  return pow(sse / n, 0.5);
}
';

src.SymmetricMF.NonzeroCounts <- '
const IntegerVector NonzeroCounts(arma::cube data) {
  IntegerVector nz(data.n_rows);
  for ( unsigned int i = 0; i < data.n_rows; i++ ) {
    for ( unsigned int j = 0; j < data.n_cols; j++ ) {
      for ( unsigned int t = 0; t < data.n_slices; t++ ) {
        if ( data(i, j, t) != 0 ) {
          nz(i)++;
        }
      }
    }
  }
  return nz;
}
';

src.SymmetricMF.UpdateU <- '
  using Eigen::Map;
  using Eigen::MatrixXd;
  using Eigen::VectorXd;
  using Eigen::LLT;
  using Eigen::Lower;

  const Map<MatrixXd> U(as<Map<MatrixXd> >(u));
  MatrixXd Unew(U);
  NumericVector X(x);
  IntegerVector arrayDims = X.attr("dim");
  arma::cube data(X.begin(), arrayDims[0], arrayDims[1], arrayDims[2], false);
  const IntegerVector nz = NonzeroCounts(data);

  const int checkRmse = as<int>(check);
  const double lambda = as<double>(lmd);
  const int k(U.cols());

  MatrixXd lMat(MatrixXd(k,k).setZero());
  for ( int i=0; i<k; i++ ) {
    lMat(i,i) = lambda;
  }
  double lastRmse = SymRmse(Unew, data);
  double curRmse = lastRmse;

  int curIndex;
  unsigned int j, t;
  for ( unsigned int i = 0; i < U.rows(); i++ ) {
    if (nz(i) >= k) {
      curIndex = 0;
      MatrixXd Unz(k, nz(i));
      VectorXd Xnz(nz(i));
    
      for ( j = 0; j < data.n_cols; j++ ) {
        for ( t = 0; t < data.n_slices; t++ ) {
          if ( data(i,j,t) != 0 ) {
            Unz.col(curIndex) = Unew.row(j);
            Xnz(curIndex) = data(i,j,t);
            curIndex++;
          }
        }
      }
      MatrixXd UtU(MatrixXd(k,k).setZero().
                   selfadjointView<Lower>().rankUpdate(Unz));
      const LLT<MatrixXd> llt(UtU + lMat);
      Unew.row(i) = llt.solve(Unz * Xnz);
      if ( checkRmse == 1 ) {
        curRmse = SymRmse(Unew, data);
        if ( curRmse > lastRmse ) {
          Unew.row(i) = U.row(i);
        }
        else { lastRmse = curRmse; }
      }
    }
  }

  List ret;
  ret["rmse"] = SymRmse(Unew, data);
  ret["obj"] = SymObjective(Unew, data, lambda);
  ret["U"] = Unew;
  return ret;
';


plug.ArmaEigen <- Rcpp:::Rcpp.plugin.maker(include.before='#include <RcppArmadillo.h>
                                                     #include <RcppEigen.h>',
                                     Depends=c("RcppEigen", "RcppArmadillo", "Rcpp"),
                                     LinkingTo=c("RcppEigen", "RcppArmadillo", "Rcpp"));

registerPlugin("RcppArmaEigen", plug.ArmaEigen);

cpp.SymmetricMF.UpdateU <-
  cxxfunction(signature(u="numeric",
                        x="numeric",
                        lmd="numeric",
                        check="numeric"),
              src.SymmetricMF.UpdateU,
              includes=c(src.SymmetricMF.Objective, src.SymmetricMF.RMSE,
                src.SymmetricMF.NonzeroCounts),
              plugin="RcppArmaEigen");


