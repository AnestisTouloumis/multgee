#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat matrix_LOR_cpp(arma::mat x,
                         double x_nrows) {
  arma::mat cumprod_rows = arma::cumprod(x, 1);
  arma::mat ans = arma::ones(x_nrows+1, x_nrows+1);
  for (int i=1; i<x_nrows+1; i++) {
      ans.submat(i, 1, i, x_nrows) =
        ans.submat(i-1, 1, i-1, x_nrows) % cumprod_rows.row(i-1);
  }
  ans = ans/accu(ans);
  return ans;
}
