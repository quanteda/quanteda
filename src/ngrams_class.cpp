#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;
using namespace quanteda;

typedef std::vector<unsigned int> Ngram;
typedef std::vector<unsigned int> Ngrams;

namespace std {
  template <>
  // Custom hash function for Ngram objects
  struct hash<Ngram> {
    std::size_t operator()(const Ngram &vec) const {
      unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
      return std::hash<unsigned int>()(seed);
    }
  };
}

class ngramMaker {

public:
  ngramMaker(){};
      
  List MakeNgramVector(NumericVector tokens, NumericVector ns, NumericVector skips){
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    Ngrams ngrams = ngram(tokens, ns, skips);
    return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                              Rcpp::Named("id_unigram") = dump());
  }
  
  List MakeNgramList(List texts, NumericVector ns, NumericVector skips) {
      
    // Itterate over documents
    List texts_ngram(texts.size());
    for (int h = 0; h < texts.size(); h++){
      Rcpp::checkUserInterrupt(); // allow user to stop
      texts_ngram[h] = ngram(texts[h], ns, skips);
    }
    return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                              Rcpp::Named("id_unigram") = dump());
  }
  
  XPtr<Tokens> MakeNgramListPtr(List texts, NumericVector ns, NumericVector skips) {
    
    // Itterate over documents
    List texts_ngram(texts.size());
    for (int h = 0; h < texts.size(); h++){
      Rcpp::checkUserInterrupt(); // allow user to stop
      texts_ngram[h] = ngram(texts[h], ns, skips);
    }
    
    // Return pointer to Tokens object
    Tokens* tokens = new Tokens(texts_ngram, dump());
    return Rcpp::XPtr<Tokens>(tokens);
  }
  
  CharacterVector UnhashVocab(ListOf<NumericVector> ids_ngram, CharacterVector tokens, String delim){
    tokens.push_front(""); // offset tokens to match index in C++
    CharacterVector tokens_ngram(ids_ngram.size());
    for(int i=0; i < ids_ngram.size(); i++){
      tokens_ngram[i] = join_character_vector(tokens[ids_ngram[i]], delim);
    }
    return tokens_ngram;
  }
  
private:
  // Register both ngram (key) and unigram (value) IDs in a hash table
  std::unordered_map<Ngram, unsigned int> map_ngram;
  
  int ngram_id(Ngram ngram){
    
    // Add new ID without multiple access
    unsigned int &id_ngram = map_ngram[ngram];
    if(id_ngram){
      //Rcout << "Old " << id_ngram << ": ";
      //dev::print_ngram_hashed(ngram);
      return id_ngram;
    }
    id_ngram = map_ngram.size();
    //Rcout << "New " << id_ngram << ": ";
    //dev::print_ngram_hashed(ngram);
    return id_ngram;
  }
  
  void skip(NumericVector &tokens,
            unsigned int start,
            unsigned int n, 
            NumericVector skips,
            Ngram ngram,
            Ngrams &ngrams,
            int pos_tokens, int &pos_ngrams
  ){
    
    ngram[pos_tokens] = tokens[start];
    pos_tokens++;
    
    //Rcout << "Token " << tokens[start] << "\n";
    if(pos_tokens < n){
      for (int j = 0; j < skips.size(); j++){
        int next = start + skips[j];
        if(next < 0 || tokens.size() - 1 < next) break;
        if(tokens[next] == 0) break; // exclude padding
        //Rcout << "Join " << ngram << " at " << pos_tokens << " " << next << "\n";
        skip(tokens, next, n, skips, ngram, ngrams, pos_tokens, pos_ngrams);
      }
    }else{
      //Rcout << "Add " << ngram << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
      ngrams[pos_ngrams] = ngram_id(ngram);
      pos_tokens = 0;
      pos_ngrams++;
    }
  }
  
  
  Ngrams ngram(NumericVector tokens, NumericVector ns, NumericVector skips) {
    
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
        if(tokens[start] == 0) continue; // exclude padding
        skip(tokens, start, n, skips, ngram, ngrams, pos_tokens, pos_ngrams); // get ngrams as reference
      }
    }
    ngrams.resize(pos_ngrams - 1);
    return ngrams;
  }
  
  List dump(){
    // Separate key and values of unordered_map
    List ids_unigram(map_ngram.size());
    for (std::pair<Ngram, unsigned int> iter : map_ngram){
      //Rcout << "ID " << to_string(iter.second) << ": ";
      //print_ngram_hashed(iter.first);
      ids_unigram[iter.second - 1] = iter.first;
    }  
    return ids_unigram;
  }
  
};



// Expose C++ class to R
RCPP_MODULE(ngram_module) {
  class_<ngramMaker>("ngramMaker")
  .constructor()
  .method("generate_vector", &ngramMaker::MakeNgramVector)
  .method("generate_list", &ngramMaker::MakeNgramList)
  .method("generate_list_ptr", &ngramMaker::MakeNgramListPtr)
  .method("unhash_vocab", &ngramMaker::UnhashVocab)
  ;
}


/*** R

nm <- new(ngramMaker)

chars <- rep(head(letters), 2)
types <- unique(chars)
chars_hashed <- match(chars, types)
res <- nm$generate_vector(chars_hashed, 3, 1)

ngram <- res$ngram
ngram_ids <- res$id_ngram

vocaburary <- sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-')
vocaburary[ngram]

microbenchmark::microbenchmark(
  sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-'),
  nm$unhash_vocab(res$id_unigram, types, "-")
)

tokens <- tokenize(c('a b c d e', 'c d e f g'))
tokens_hashed <- hashTokens(tokens)
res <- nm$generate_list(tokens_hashed, 2, 0:1)
ptr <- nm$generate_list_ptr(tokens_hashed, 2, 0:1)




*/

