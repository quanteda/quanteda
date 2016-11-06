#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List split_df_cpp(DataFrame df) {
  int len_cols=df.size();
  List cols(len_cols);
  for (int i=0; i < len_cols; i++) {
    CharacterVector column = df[i] ;
    cols[i] = column ;
  }
  return cols;
}

// [[Rcpp::export]]
Rcpp::List deepcopy(Rcpp::List x){
  Rcpp::List y = clone(x);
  return y;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
#df <- expand.grid(LETTERS[1:3], LETTERS[4:5], LETTERS[6], stringsAsFactors=FALSE)
#split_df_cpp(df)
*/
