#include <Rcpp.h>
#include <vector>
#include <algorithm>  
#include "quanteda.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


// [[Rcpp::export]]
int qatd_cpp_count_hash_vector(NumericVector tokens_, 
                                           NumericVector seq_){
  
  std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
  std::vector<int> seq = Rcpp::as< std::vector<int> >(seq_);
  int len_seq = seq.size();
  std::vector<int>::iterator it;
  it = std::search(tokens.begin(), tokens.end(), seq.begin(), seq.end());
  //Rcout << std::distance(tokens.begin(), it) << "\n";
  int count = 0;
  while(it != tokens.end()){
    count += 1;
    std::advance(it, seq.size());
    it = std::search(it, tokens.end(), seq.begin(), seq.end());
    //Rcout << std::distance(tokens.begin(), it) << "\n";
  }
  return count;

}

// [[Rcpp::export]]
NumericVector qatd_cpp_count_hash_list(List texts_, 
                              std::vector<bool> flags,
                              NumericVector seq){
  //Rcout << id << "\n";
  List texts = clone(texts_);
  NumericVector counts(texts.size(), 0);
  int len = texts.size();
  if(flags.size() != len){
    Rcout << "Invalid flag is given\n";
    return(counts);
  }
  for (int h = 0; h < len; h++){
    if(flags[h]){
      //Rcout << "Text " << h << "\n";
      counts[h] = qatd_cpp_count_hash_vector(texts[h], seq);
    }
  }
  return(counts);
}

/***R
toks_hash <- rep(1:26, 1000)
qatd_cpp_count_hash_vector(toks_hash, c(3, 4, 5))

microbenchmark::microbenchmark(
  qatd_cpp_count_hash_vector(toks_hash, c(3, 4)),
  quanteda:::matchSequence(c(3, 4), toks_hash), times=100
)

*/
