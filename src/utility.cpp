#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
std::vector<CharacterVector> split_df_cpp(DataFrame df) {
    int len_cols=df.size();
    std::vector<CharacterVector> cols(len_cols);
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
List qatd_cpp_remove_chr_list(List list_, String elem_remove){
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
    if(j > 0){
      list[h] = elems_new[seq(0, j - 1)];
    }else{
      list[h] = CharacterVector(0);
    }
  }
  return list;
}

// [[Rcpp::export]]
List qatd_cpp_remove_int_list(List list_, int elem_remove){
  List list = clone(list_);
  for(int h=0; h < list.size(); h++){
    IntegerVector elems = list[h];
    IntegerVector elems_new(elems.size());
    int j = 0;
    for(int i=0; i < elems.size(); i++){
      if(elems[i] != elem_remove){
        elems_new[j] = elems[i];
        j++;
      }
    }
    if(j > 0){
      list[h] = elems_new[seq(0, j - 1)];
    }else{
      list[h] = IntegerVector(0);
    }
  }
  return list;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

# df <- expand.grid(LETTERS[1:3], LETTERS[4:5], LETTERS[6], stringsAsFactors=FALSE)
# qatd_cpp_split_df(df)
*/
