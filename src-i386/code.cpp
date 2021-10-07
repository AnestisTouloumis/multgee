#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat ipfp_cpp(arma::vec initial_table,
                   arma::colvec row_marginals,
                   arma::rowvec col_marginals,
                   int dimension,
                   int maxiter,
                   double tolerance) {
  arma::mat ans = reshape(initial_table, dimension, dimension);
  arma::colvec row_sums = sum(ans, 1);
  for (int i=0; i<maxiter; i++) {
    ans.each_col() %= row_marginals/row_sums;
    arma::rowvec col_sums = sum(ans, 0);
    double criterion = max(abs(col_sums - col_marginals));
    if (criterion <= tolerance) break;
    ans.each_row() %= col_marginals/col_sums;
    row_sums = sum(ans, 1);
    criterion = max(abs(row_sums - row_marginals));
    if (criterion <= tolerance) break;
  }
  return ans;
}
