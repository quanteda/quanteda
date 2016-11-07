#include <Rcpp.h>
#include <vector>
#include <algorithm>  
#include "quanteda.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


// [[Rcpp::export]]
NumericVector qatd_cpp_replace_hash_vector(NumericVector tokens_, 
                                           NumericVector seq_,
                                           int id){
  
  std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
  std::vector<int> seq = Rcpp::as< std::vector<int> >(seq_);
  int len_seq = seq.size();
  std::vector<int>::iterator it;
  it = std::search(tokens.begin(), tokens.end(), seq.begin(), seq.end());
  //Rcout << std::distance(tokens.begin(), it) << "\n";
  bool match = false;
  while(it != tokens.end()){
    match = true;
    std::fill(it, std::min(it + len_seq, tokens.end()), 0);
    *it = id;
    std::advance(it, seq.size());
    it = std::search(it, tokens.end(), seq.begin(), seq.end());
    //Rcout << std::distance(tokens.begin(), it) << "\n";
  }
  if(match) tokens.erase(std::remove(tokens.begin(), tokens.end(), 0), tokens.end());
  return wrap(tokens);

}

// [[Rcpp::export]]
List qatd_cpp_replace_hash_list(List texts_, 
                                std::vector<bool> flags,
                                NumericVector seq,
                                int id){
  //Rcout << id << "\n";
  List texts = clone(texts_);
  int len = texts.size();
  if(flags.size() != len){
    Rcout << "Invalid flag is given\n";
    return(texts);
  }
  for (int h = 0; h < len; h++){
    if(flags[h]){
      //Rcout << "Text " << h << "\n";
      texts[h] = qatd_cpp_replace_hash_vector(texts[h], seq, id);
    }
  }
  return(texts);
}

/***R
# toks <- rep(letters, 1000)
# toks_hash <- rep(1:26, 1000)
# 
# qatd_cpp_replace_hash_vector(toks_hash, c(3, 4, 5), 9999)

# microbenchmark::microbenchmark(
#   qatd_cpp_replace_hash_vector(toks_hash, c(3, 4), 9999),
#   join_tokens_cpp(toks, c('c', 'd'), '_'),
#   quanteda:::matchSequence(c(3, 4), toks_hash), times=100
# )

*/
