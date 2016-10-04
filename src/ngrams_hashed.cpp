#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
#include "dev.h"
#include "quanteda.hpp"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;
using namespace quanteda;

typedef std::vector<unsigned int> Ngram;
typedef std::vector<unsigned int> Ngrams;

namespace std {
  template <>

  // Custom hash function for Ngram objects
  struct hash<Ngram>
  {
    std::size_t operator()(const Ngram &vec) const
    {
      unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
      return std::hash<unsigned int>()(seed);
    }
  };
}

int ngram_id(Ngram ngram,
             std::unordered_map<Ngram, unsigned int> &map_ngram){
  
  // Add new ID without multiple access
  unsigned int &id_ngram = map_ngram[ngram];
  if(id_ngram){
      //Rcout << "Old " << id_ngram << ": ";
      //print_ngram_hashed(ngram);
      return id_ngram;
  }
  id_ngram = map_ngram.size() + 1;
  id_ngram = id_ngram - 1; // offset to start ID from 1
  //Rcout << "New " << id_ngram << ": ";
  //print_ngram_hashed(ngram);
  return id_ngram;
}

void skip_hashed(NumericVector &tokens,
          unsigned int start,
          unsigned int n, 
          NumericVector skips,
          Ngram ngram,
          Ngrams &ngrams,
          std::unordered_map<Ngram, unsigned int> &map_ngram,
          int pos_tokens, int &pos_ngrams
){
    
    ngram[pos_tokens] = tokens[start];
    pos_tokens++;
    
    //Rcout << "Token " << tokens[start] << "\n";
    if(pos_tokens < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next < 0 || tokens.size() - 1 < next) break;
            //Rcout << "Join " << ngram << " at " << pos_tokens << " " << next << "\n";
            skip_hashed(tokens, next, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams);
        }
    }else{
        //Rcout << "Add " << ngram << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
        ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
        pos_tokens = 0;
        pos_ngrams++;
    }
}


Ngrams skipgram_hashed(NumericVector tokens,
                       NumericVector ns, 
                       NumericVector skips,
                       std::unordered_map<Ngram, unsigned int> &map_ngram) {
    
    int pos_tokens = 0; // Position in tokens
    int pos_ngrams = 0; // Position in ngrams
    
    // Pre-allocate memory
    int size_reserve = 0;
    for (int k = 0; k < ns.size(); k++) {
        size_reserve += std::pow(skips.size(), ns[k]) * tokens.size();
    }
    Ngrams ngrams(size_reserve);
    
    // Generate skipgrams recursively
    for (int k = 0; k < ns.size(); k++) {
        int n = ns[k];
        Ngram ngram(n);
        for (int start = 0; start < tokens.size() - (n - 1); start++) {
            skip_hashed(tokens, start, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams); // Get ngrams as reference
        }
    }
    ngrams.resize(pos_ngrams - 1);
    return ngrams;
}

// [[Rcpp::export]]
List qatd_cpp_ngram_hashed_vector(NumericVector tokens,
                                  NumericVector ns, 
                                  NumericVector skips){
  
  // Register both ngram (key) and unigram (value) IDs in a hash table
  std::unordered_map<Ngram, unsigned int> map_ngram;
  Ngrams ngrams = skipgram_hashed(tokens, ns, skips, map_ngram);
  
  // Separate key and values of unordered_map
  List ids_unigram(map_ngram.size());
  for (std::pair<Ngram, unsigned int> iter : map_ngram){
    //Rcout << "ID " << to_string(iter.second) << ": ";
    //print_ngram_hashed(iter.first);
    ids_unigram[iter.second - 1] = iter.first;
  }
  
  return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                            Rcpp::Named("id_unigram") = ids_unigram);
}

// [[Rcpp::export]]
List qatd_cpp_ngram_hashed_list(List texts,
                                   NumericVector ns,
                                   NumericVector skips) {

  // Register both ngram (key) and unigram (value) IDs in a hash table
  std::unordered_map<Ngram, unsigned int> map_ngram;
  
  // Itterate over documents
  List texts_ngram(texts.size());
  for (int h = 0; h < texts.size(); h++){
      texts_ngram[h] = skipgram_hashed(texts[h], ns, skips, map_ngram);
  }
  
  // Separate key and values of unordered_map
  List ids_unigram(map_ngram.size());
  for (std::pair<Ngram, unsigned int> iter : map_ngram){
    //Rcout << "ID " << to_string(iter.second) << ": ";
    //print_ngram_hashed(iter.first);
    ids_unigram[iter.second - 1] = iter.first;
  }

  return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                            Rcpp::Named("id_unigram") = ids_unigram);
}


/*** R

tokens <- tokenize(c('a b c d e', 'c d e f g'))
tokens_hashed <- hashTokens(tokens)
res <- qatd_cpp_ngram_hashed_list(tokens_hashed, 2, 0:1)
res$text

# tokens <- rep(head(letters), 2)
# types <- unique(tokens)
# tokens_hashed <- match(tokens, types)
# res <- qatd_cpp_ngram_hashed_vector(tokens_hashed, 1:4, 1:5)
# ngram <- res$ngram
# ngram_ids <- res$id_ngram
# ngram_types <- sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-')
# names(ngram_ids) <- ngram_types
# names(ngram_ids[match(ngram, ngram_ids)])
*/

