#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat diagmod2(arma::vec x) {
  arma::mat ans = diagmat(x);
  return ans;
}

