#include <Rcpp.h>
#include <string>
#include <algorithm>
#include <unordered_map>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;

//typedef std::vector<unsigned int> UnsignedIntegerVector;
namespace std {
  template <>
  struct hash<NumericVector>
  {
    std::size_t operator()(const NumericVector& vec) const
    {
      // Hash function for NmericVector
      // See http://stackoverflow.com/questions/17016175
      std::size_t hash = 17;
      for(auto& elm : vec) {
        hash = 31 * hash + std::hash<int>()(elm);
      }
      //Rcout << "Hash " << ": "<< hash << "\n";
      return hash;
    }
  };
}


int generate(NumericVector ngram,
             std::unordered_map<unsigned int, NumericVector> &map_ngram){
  
  // Hash function for NmericVector
  // See http://stackoverflow.com/questions/17016175
  std::size_t id_ngram = 17;
  for(auto& id_token : ngram) {
    id_ngram = 31 * id_ngram + std::hash<int>()(id_token);
  }
  map_ngram[id_ngram] = ngram;
  //Rcout << "ID " << ": "<< id_ngram << "\n";
  return id_ngram;
  
}

void skip_hashed(NumericVector &tokens,
          unsigned int start,
          unsigned int n, 
          NumericVector ks,
          NumericVector ngram,
          NumericVector &ngrams,
          std::unordered_map<unsigned int, NumericVector> &map_ngram
){
    
    int len_tokens = tokens.size();
    int len_ks = ks.size();
    
    ngram.push_back(tokens[start]);
    //Rcout << "Token " << tokens[start] << "\n";
    if(ngram.size() < n){
        for (int j = 0; j < len_ks; j++){
            int next = start + ks[j];
            if(next > len_tokens - 1) break;
            skip_hashed(tokens, next, n, ks, ngram, ngrams, map_ngram);
        }
    }else{
        int id_ngram = generate(ngram, map_ngram);
        ngrams.push_back(id_ngram);
    }
}

// [[Rcpp::export]]
List skipgramcpp_hashed(NumericVector tokens,
                                NumericVector ns, 
                                NumericVector ks) {
    
    // Generate skipgrams recursively
    NumericVector ngram;
    NumericVector ngrams; // For the recursive function
    std::unordered_map<unsigned int, NumericVector> map_ngram;
    
    int len_ns = ns.size();
    int len_tokens = tokens.size();
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        for (int h = 0; h < len_tokens; h++) {
          skip_hashed(tokens, h, n, ks, ngram, ngrams, map_ngram); // Get ngrams as reference
        }
    }
    
    // Separate key and values of unordered_map
    NumericVector ids_ngram;
    List tokens_ngram;
    for (std::pair<unsigned int, NumericVector> iter : map_ngram){
      //Rcout << " " << iter.first << ":" << iter.second << "\n";
      ids_ngram.push_back(iter.first);
      tokens_ngram.push_back(iter.second);
    }
    
    return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                              Rcpp::Named("id") = ids_ngram,
                              Rcpp::Named("token") = tokens_ngram);
} 

/*** R
tokens <- rep(letters[1:5], 20)
types <- unique(tokens)
toks_hash <- match(tokens, types)

#microbenchmark::microbenchmark(skipgramcpp(tokens, 2:3, 1:2, '-'),
#                               skipgramcpp_hashed(toks_hash, 2:3, 1:2))

res <- skipgramcpp_hashed(toks_hash, 2:3, 1:2)
ngram <- res$ngram
ngram_ids <- res$id
ngram_types <- unlist(lapply(res$token, function(x, y, z) paste(y[x], collapse=z) , types, '-'))
names(ngram_ids) <- ngram_types
names(ngram_ids[match(ngram, ngram_ids)])
*/

