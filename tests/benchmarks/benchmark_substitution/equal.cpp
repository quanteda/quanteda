#define RCPP_USING_CXX11
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// [[Rcpp::export]]
CharacterVector equal_cpp_chr(CharacterVector tokens_,
                          String type_){
  
  CharacterVector tokens = clone(tokens_);
  String type = type_;
  for(int i=0; i < tokens.size(); i++){
    if(tokens[i] == type){
      tokens[i] = "";
    }
  }
  return tokens;
}


// [[Rcpp::export]]
NumericVector equal_cpp_num(NumericVector tokens_,
                            int type_){
  
  NumericVector tokens = clone(tokens_);
  int type = type_;
  for(int i=0; i < tokens.size(); i++){
    if(tokens[i] == type){
      tokens[i] = 0;
    }
  }
  return Rcpp::wrap(tokens);
}


/*** R

# toks <- rep(letters, 100)
# toks_hash <- rep(1:26, 100)
# microbenchmark::microbenchmark(
#   equal_cpp_chr(toks, 'd'),
#   equal_cpp_num(toks_hash, 4),
#   equal_cpp_num(toks_hash, 4L),
#   times = 100
# )
# 
# identical(equal_r(toks_hash, 'd'), equal_cpp(toks_hash, 'd'))
# identical(equal_r_num(toks_hash, 4), equal_cpp_num(toks_hash, 4))

  
*/
