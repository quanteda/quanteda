#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;

typedef std::vector<unsigned int> Ngram;

namespace std {
  template <>

  // Custom hash function for Ngram objects
  struct hash<Ngram>
  {
    std::size_t operator()(const Ngram &vec) const
    {
      unsigned int key = std::accumulate(vec.begin(), vec.end(), 0);
      return std::hash<unsigned int>()(key);
    }
  };
}


int ngram_id(Ngram ngram,
             std::unordered_map<Ngram, unsigned int> &map_ngram){
  
  // Add new Id without multiple access
  unsigned int &id_ngram = map_ngram[ngram];
  if(id_ngram){
      return id_ngram;
  }
  id_ngram = map_ngram.size() + 1;
  return id_ngram;
}

void skip_hashed(NumericVector &tokens,
          unsigned int start,
          unsigned int n, 
          NumericVector skips,
          Ngram ngram,
          NumericVector &ngrams,
          std::unordered_map<Ngram, unsigned int> &map_ngram,
          int e, int &f
){
    
    ngram[e] = tokens[start];
    e++;
    
    //Rcout << "Token " << tokens[start] << "\n";
    if(e < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next < 0 || tokens.size() - 1 < next) break;
            //Rcout << "Join " << ngram << " at " << e << " " << next << "\n";
            skip_hashed(tokens, next, n, skips, ngram, ngrams, map_ngram, e, f);
        }
    }else{
        //Rcout << "Add " << ngram << " at " << f << "/" << ngrams.size() << "\n";
        ngrams[f] = ngram_id(ngram, map_ngram);
        e = 0;
        f++;
    }
}


NumericVector skipgramcpp_hashed(NumericVector tokens,
                                 NumericVector ns, 
                                 NumericVector skips,
                                 std::unordered_map<Ngram, unsigned int> &map_ngram) {
    
    int e = 0; // Local index for unigrams in ngram
    int f = 0; // Global index for generated ngrams 
    NumericVector ngrams(std::pow(tokens.size(), ns.size())); // Pre-allocate memory
    
    // Generate skipgrams recursively
    for (int g = 0; g < ns.size(); g++) {
        int n = ns[g];
        Ngram ngram(n);
        for (int start = 0; start < tokens.size() - (n - 1); start++) {
            skip_hashed(tokens, start, n, skips, ngram, ngrams, map_ngram, e, f); // Get ngrams as reference
        }
    }
    return ngrams[seq(0, f - 1)];
}

// [[Rcpp::export]]
List qatd_cpp_ngram_hashed_vector(NumericVector tokens,
                                     NumericVector ns, 
                                     NumericVector skips){
  
  // Register both ngram (key) and unigram (value) IDs in a hash table
  std::unordered_map<Ngram, unsigned int> map_ngram;
  NumericVector ngrams = skipgramcpp_hashed(tokens, ns, skips, map_ngram);
  
  // Separate key and values of unordered_map
  NumericVector ids_ngram(map_ngram.size());
  List ids_unigram(map_ngram.size());
  int k = 0;
  for (std::pair<Ngram, unsigned int> iter : map_ngram){
    ids_unigram[k] = iter.first;
    ids_ngram[k] = iter.second;
    k++;
  }
  
  return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                            Rcpp::Named("id_ngram") = ids_ngram,
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
      texts_ngram[h] = skipgramcpp_hashed(texts[h], ns, skips, map_ngram);
  }
  
  // Separate key and values of unordered_map
  NumericVector ids_ngram(map_ngram.size());
  List ids_unigram(map_ngram.size());
  int k = 0;
  for (std::pair<Ngram, unsigned int> iter : map_ngram){
    ids_unigram[k] = iter.first;
    ids_ngram[k] = iter.second;
    k++;
  }

  return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                            Rcpp::Named("id_ngram") = ids_ngram,
                            Rcpp::Named("id_unigram") = ids_unigram);
}


/*** R


tokens <- rep(head(letters), 2)
types <- unique(tokens)
tokens_hashed <- match(tokens, types)

#microbenchmark::microbenchmark(skipgramcpp(tokens, 2:3, 1:2, '-'),
#                               qatd_cpp_ngram_hashed_vector(tokens_hashed, 2:3, 1:2))

res <- qatd_cpp_ngram_hashed_vector(tokens_hashed, 1:5, 1:2)
ngram <- res$ngram
ngram_ids <- res$id_ngram
ngram_types <- unlist(lapply(res$id_unigram, function(x, y, z) paste(y[x], collapse=z) , types, '-'))
names(ngram_ids) <- ngram_types
names(ngram_ids[match(ngram, ngram_ids)])
*/

