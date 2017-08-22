#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat ipfp2(arma::mat initial, arma::vec rowmars, arma::vec colmars, 
                double maxit, double tol) {
  arma::mat ans = initial;
  arma::vec rowsums = vectorise(sum(ans,1));
  arma::vec colsums = vectorise(sum(ans,0));
  arma::vec rowweights = rowmars/rowsums;
  arma::vec colweights = colmars/colsums;
  double crit = tol + 1;
  for (int i=0; i < maxit; i++)
  {
    rowweights = rowmars/rowsums;
    ans = diagmat(rowweights) * ans;
    colsums = vectorise(sum(ans,0));
    crit = max(abs(colsums - colmars));
    if(crit<=tol) i=maxit;
    colweights = colmars/colsums;
    ans =  ans * diagmat(colweights);
    rowsums = vectorise(sum(ans,1));
    crit = max(abs(rowsums - rowmars));
    if(crit<=tol) i=maxit;
  }  
  return ans;
}