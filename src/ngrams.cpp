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

int ngram_id(Ngram ngram,
             std::unordered_map<Ngram, unsigned int> &map_ngram){
  
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
            //if(next > tokens.size() - 1) break;
            //Rcout << "Join " << tokens[start] << " at " << pos_tokens << " " << next << "\n";
            skip_hashed(tokens, next, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams);
        }
    }else{
        ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
        //Rcout << "Add " << ngrams[pos_ngrams] << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
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
    ngrams.resize(pos_ngrams);
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
      Rcpp::checkUserInterrupt(); // allow user to stop
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

// [[Rcpp::export]]
CharacterVector qatd_cpp_ngram_unhash_type(ListOf<NumericVector> ids_ngram, 
                                            CharacterVector tokens, String delim){
  tokens.push_front(""); // offset tokens to match index in C++
  CharacterVector tokens_ngram(ids_ngram.size());
  for(int i=0; i < ids_ngram.size(); i++){
    tokens_ngram[i] = join_character_vector(tokens[ids_ngram[i]], delim);
  }
  return tokens_ngram;
}


void skip(std::vector< std::string > &tokens,
          unsigned int start,
          unsigned int n, 
          std::vector< int > ks,
          std::vector<std::string> ngram,
          std::vector< std::vector<std::string> > &ngrams
){
    
    int len_tokens = tokens.size();
    int len_ks = ks.size();
    
    ngram.push_back(tokens[start]);
    if(ngram.size() < n){
        for (int j = 0; j < len_ks; j++){
            int next = start + ks[j];
            if(next > len_tokens - 1) break;
            skip(tokens, next, n, ks, ngram, ngrams);
        }
    }else{
        ngrams.push_back(ngram);
    }
}

// [[Rcpp::export]]
StringVector skipgramcpp(std::vector < std::string > tokens,
                         std::vector < int > ns, 
                         std::vector < int > ks, 
                         std::string delim) {
    
    // Generate skipgrams recursively
    std::vector<std::string> ngram;
    std::vector< std::vector<std::string> > ngrams; // For the recursive function
    int len_ns = ns.size();
    int len_tokens = tokens.size();
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        for (int h = 0; h < len_tokens; h++) {
            skip(tokens, h, n, ks, ngram, ngrams); // Get ngrams as reference
        }
    }
    
    // Join elements of ngrams
    StringVector tokens_ngram;
    int len_ngrams = ngrams.size();
    for (int h = 0; h < len_ngrams; h++) {
        Rcpp::String str(join_vector(ngrams[h], delim));
        str.set_encoding(CE_UTF8);
        tokens_ngram.push_back(str);
    }
    return tokens_ngram;
}



/*** R

txt <- c('a b c d e', 'c d e f g')
toks <- tokens(txt, hasn=FALSE)
toks_hash <- tokens(txt, hasn=TRUE)
res <- qatd_cpp_ngram_hashed_list(toks_hash, 3, 1)
res$text

toks_char <- rep(letters, 100)
toks_int <- rep(1:26, 100)

microbenchmark::microbenchmark(
  old=skipgramcpp(toks_char, 3, 1, '-'),
  new=qatd_cpp_ngram_hashed_vector(toks_int, 3, 1), 
  unit='relative'
)

# tokens <- rep(head(letters), 2)
# types <- unique(tokens)
# tokens_hashed <- match(tokens, types)
# res <- qatd_cpp_ngram_hashed_vector(tokens_hashed, 3, 1)
# ngram <- res$ngram
# ngram_ids <- res$id_ngram
# vocaburary <- sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-')
# vocaburary[ngram]
# 
# microbenchmark::microbenchmark(
#   sapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-'),
#   qatd_cpp_ngram_unhash_type(res$id_unigram, types, '-'),
#   unit='relative'
# )

*/

