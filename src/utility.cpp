#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_split_df(DataFrame df) {
  int len_cols=df.size();
  List cols(len_cols);
  for (int i=0; i < len_cols; i++) {
    CharacterVector column = df[i] ;
    cols[i] = column ;
  }
  return cols;
}

// [[Rcpp::export]]
Rcpp::List qatd_cpp_deepcopy(Rcpp::List x){
  Rcpp::List y = clone(x);
  return y;
}

// [[Rcpp::export]]
List qatd_cpp_remove_string_list(List list_, String elem_remove){
  List list = clone(list_);
  for(int h=0; h < list.size(); h++){
    CharacterVector elems = list[h];
    CharacterVector elems_new(elems.size());
    int j = 0;
    for(int i=0; i < elems.size(); i++){
      if(elems[i] != elem_remove){
        elems_new[j] = elems[i];
        j++;
      }
    }
    list[h] = elems_new[seq(0, j - 1)];
  }
  return(list);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

#df <- expand.grid(LETTERS[1:3], LETTERS[4:5], LETTERS[6], stringsAsFactors=FALSE)
#split_df_cpp(df)
*/
