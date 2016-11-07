#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_split_df(DataFrame df) {
  List cols(df.size());
  for (int i=0; i < df.size(); i++) {
    CharacterVector column = df[i] ;
    cols[i] = column ;
  }
  return cols;
}

// [[Rcpp::export]]
List qatd_cpp_deepcopy(List x_){
  List x = clone(x_);
  return x;
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
  return list;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

#df <- expand.grid(LETTERS[1:3], LETTERS[4:5], LETTERS[6], stringsAsFactors=FALSE)
#split_df_cpp(df)
*/
