#include <Rcpp.h>
#include <vector>
#include <algorithm>  
#include "quanteda.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


// [[Rcpp::export]]
IntegerVector qatd_cpp_detect_hash_vector(IntegerVector tokens_, 
                                          IntegerVector tokens_loc_, 
                                          IntegerVector seq_,
                                          int id){
  
  std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
  std::vector<int> tokens_loc = Rcpp::as< std::vector<int> >(tokens_loc_);
  std::vector<int> seq = Rcpp::as< std::vector<int> >(seq_);
  int len_seq = seq.size();
  std::vector<int>::iterator it;
  it = std::search(tokens.begin(), tokens.end(), seq.begin(), seq.end());
  //Rcout << std::distance(tokens.begin(), it) << "\n";
  bool match = false;
  while(it != tokens.end()){
    int loc = std::distance(tokens.begin(), it);
    tokens_loc[loc] = id;
    std::advance(it, seq.size());
    it = std::search(it, tokens.end(), seq.begin(), seq.end());
    //Rcout << loc << "\n";
  }
  return wrap(tokens_loc);

}

// [[Rcpp::export]]
List qatd_cpp_detect_hash_list(List texts_, 
                               SEXP texts_loc_,
                               std::vector<bool> flags,
                               IntegerVector seq,
                               int id){

  List texts = texts_;
  List texts_loc = texts_loc_;
  
  // Generate empty List
  if(texts_loc_ == R_NilValue){
    //Rcout << "Is NULL\n";
    for(int g=0; g<texts.size(); g++){
      IntegerVector text = texts[g];
      texts_loc.push_back(IntegerVector(text.size()));
    }
  }
  int len = texts.size();
  if(flags.size() != len){
    Rcout << "Invalid flag is given\n";
    return(texts);
  }
  for (int h = 0; h < len; h++){
    if(flags[h]){
      //Rcout << "Text " << h << "\n";
      texts_loc[h] = qatd_cpp_detect_hash_vector(texts[h], texts_loc[h], seq, id);
    }
  }
  return(texts_loc);
}

/***R

toks_hash <- list(rep(1:10, 10), rep(5:15, 10))
loc <- qatd_cpp_detect_hash_list(toks_hash, NULL, rep(TRUE, length(toks_hash)), c(1, 2), 99)
loc <- qatd_cpp_detect_hash_list(toks_hash, loc, rep(TRUE, length(toks_hash)), c(5, 6), 88)
loc


# microbenchmark::microbenchmark(
#   qatd_cpp_replace_hash_vector(toks_hash, c(3, 4), 9999),
#   join_tokens_cpp(toks, c('c', 'd'), '_'),
#   quanteda:::matchSequence(c(3, 4), toks_hash), times=100
# )

*/
