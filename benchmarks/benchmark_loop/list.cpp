#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
ListOf<CharacterVector> loop_defined_chr(ListOf<CharacterVector> texts,
                                          CharacterVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

// [[Rcpp::export]]
ListOf<NumericVector> loop_defined_int(ListOf<NumericVector> texts,
                                       NumericVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

// [[Rcpp::export]]
List loop_chr(List texts,
              CharacterVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

// [[Rcpp::export]]
List loop_int(List texts,
              NumericVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

/*** R
# numbers <- 1:26
# toks <- rep(list(LETTERS), 100000)
# toks_hash <- rep(list(numbers), 100000)

#loopundefined(toks, letters)
#loop_defined_chr(toks, letters)

*/
