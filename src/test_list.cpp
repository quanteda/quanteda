#include <Rcpp.h>
using namespace Rcpp;


// template <typename WHAT>
// class ListOf : public List {
// public:
//   template <typename T>
//   ListOf( const T& x) : List(x){}
//   WHAT operator[](int i){ return as<WHAT>( ( (List*)this)->operator[]( i) ) ; }
//   
// };

// [[Rcpp::export]]
ListOf<CharacterVector> list_defined_charactor(ListOf<CharacterVector> texts,
                                     CharacterVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

// [[Rcpp::export]]
ListOf<NumericVector> list_defined_numeric(ListOf<NumericVector> texts,
                                           NumericVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

// [[Rcpp::export]]
List list_undefined(List texts,
                    CharacterVector tokens){
  for(int i=0; i < texts.size(); i++){
    texts[i] = tokens;
    //Rcout << i << "\n";
  }
  return texts;
}

/*** R
numbers <- 1:26
toks <- rep(list(LETTERS), 100000)
toks_numeric <- rep(list(numbers), 100000)
microbenchmark::microbenchmark(list_undefined(toks, letters),
                               list_defined_numeric(toks_numeric, numbers),
                               list_defined_charactor(toks, letters))
toks_out1 <- list_undefined(toks, letters)
object.size(toks_out1)
toks_out2 <- list_defined_charactor(toks, letters)
object.size(toks_out2)
*/
