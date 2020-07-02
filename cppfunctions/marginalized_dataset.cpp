#include <Rcpp.h>
// [[Rcpp::export]]
Rcpp::DataFrame marginalized_dataset_cpp(Rcpp::IntegerVector response,
                                         Rcpp::IntegerVector id,
                                         Rcpp::IntegerVector repeated) {
  // long to wide format of responses
  // zeros indicate NA values
  int categories_no = max(response);
  int sample_size = max(id);
  int times_no = max(repeated);
  Rcpp::IntegerMatrix wide_responses(sample_size, times_no);
  for(int i=1; i<sample_size+1; ++i) {
    Rcpp::IntegerVector response_i = response[id == i];
    Rcpp::IntegerVector repeated_i = repeated[id == i];
    for(int j=1; j<repeated_i.size()+1; ++j) {
      wide_responses(i - 1, repeated_i(j - 1) - 1) = response_i(j - 1);
    }
  }
  // calculating the marginalized counts
  int time_pairs_no = times_no * (times_no - 1) / 2;
  Rcpp::IntegerVector counts(pow(categories_no, 2) * time_pairs_no);
  int k = 1;
  for(int l=1; l<sample_size+1; ++l) {
    k = 1;
    for(int categ_one=1; categ_one<categories_no+1; categ_one++) {
      for(int categ_two=1; categ_two<categories_no+1; categ_two++) {
        for(int i=1; i<times_no; ++i){
          if(wide_responses(l - 1, i - 1) == categ_one) {
            for(int j=i+1; j<times_no+1; ++j) {
              if(wide_responses(l - 1, j - 1) == categ_two) {
                counts(k - 1) += 1;
              }
              k += 1;
            }
          } else {
            k += times_no - i;
          }
        }
      }
    }
  }
  // creating dataframe to fit association model 
  Rcpp::IntegerVector xx = Rcpp::seq(1, categories_no);
  Rcpp::IntegerVector x = Rcpp::rep_each(xx,
                                         time_pairs_no * categories_no);
  Rcpp::IntegerVector y = Rcpp::rep(Rcpp::rep_each(xx, time_pairs_no),
                                    categories_no);
  Rcpp::IntegerVector tp = Rcpp::rep(Rcpp::seq(1, time_pairs_no),
                                     pow(categories_no, 2));
  Rcpp::DataFrame ans = Rcpp::DataFrame::create(
    Rcpp::Named("counts") = counts,
    Rcpp::Named("x") = x,
    Rcpp::Named("y") = y,
    Rcpp::Named("tp") = tp);
  return ans;
}