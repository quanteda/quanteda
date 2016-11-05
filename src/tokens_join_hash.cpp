#include <Rcpp.h>
#include <vector>
#include "quanteda.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


// [[Rcpp::export]]
NumericVector join_tokens_hash(NumericVector tokens_, 
                     NumericVector seq_,
                     int id){
  
  std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
  std::vector<int> seq = Rcpp::as< std::vector<int> >(seq_);
  //std::vector<int> seq_fill(seq.size(), 0);
  //seq_fill[0] = id;
  std::vector<int>::iterator it;
  it = std::search(tokens.begin(), tokens.end(), seq.begin(), seq.end());
  //Rcout << std::distance(tokens.begin(), it) << "\n";
  while(it != tokens.end()){
    std::advance(it, seq.size());
    //std::advance(it, 1);
    it = std::search(it, tokens.end(), seq.begin(), seq.end());
    //std::copy(seq_fill.begin(), seq_fill.end(), it);
    
    std::fill(it, it + seq.size(), 0);
    *it = id;
    //Rcout << std::distance(tokens.begin(), it) << "\n";
  }
  
  return wrap(tokens);

}

// [[Rcpp::export]]
void join_tokens_cppl(List texts, 
                      const std::vector<bool> &flags,
                      const NumericVector &seq,
                      const int id){

  int len = texts.size();
  if(flags.size() != len){
    Rcout << "Invalid flag is given\n";
    return;
  }
  for (int h = 0; h < len; h++){
    if(flags[h]){
      //Rcout << "Text " << h << "\n";
      join_tokens_hash(texts[h], seq, id);
    }
  }
}

/***R
toks <- rep(letters, 1000)
toks_hash <- rep(1:26, 1000)
microbenchmark::microbenchmark(
  join_tokens_hash(toks_hash, c(3, 4), 1000),
  quanteda:::matchSequence(c(3, 4), toks_hash), times=1
)

*/
