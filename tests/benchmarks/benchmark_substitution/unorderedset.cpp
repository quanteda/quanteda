#define RCPP_USING_CXX11
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// [[Rcpp::export]]
CharacterVector set_cpp_chr(CharacterVector tokens_,
                            CharacterVector types_){
  
  CharacterVector tokens = clone(tokens_);
  CharacterVector types = types_;
  std::unordered_set<String> set_types (types.begin(), types.end());
  for(int i=0; i < tokens.size(); i++){
    String token = tokens[i];
    bool is_in = set_types.find(token) != set_types.end();
    if(is_in == true){
      tokens[i] = "";
    }
  }
  
  return tokens;
}


// [[Rcpp::export]]
NumericVector set_cpp_num(NumericVector tokens_,
                          NumericVector types_){
  
  NumericVector tokens = clone(tokens_);
  NumericVector types = types_;
  std::unordered_set<int> set_types (types.begin(), types.end());
  for(int i=0; i < tokens.size(); i++){
    int token = tokens[i];
    bool is_in = set_types.find(token) != set_types.end();
    if(is_in == true){
      tokens[i] = 0;
    }
  }
  
  return Rcpp::wrap(tokens);
}


/*** R


# toks <- rep(letters, 100)
# toks_hash <- rep(1:26, 100)
# microbenchmark::microbenchmark(
#   set_cpp_chr(toks, c('a', 'c', 'd')),
#   set_cpp_num(toks_hash, c(1, 3, 4))
# )
#   

  
*/
