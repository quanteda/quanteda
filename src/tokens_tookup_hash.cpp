#include <Rcpp.h>
#include <vector>
#include <algorithm>  
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
#include "quanteda.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


typedef std::vector<unsigned int> Key;

namespace std {
template <>

// Custom hash function for Keys objects
struct hash<Key> {
    std::size_t operator()(const Key &vec) const {
        unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
        return std::hash<unsigned int>()(seed);
    }
};
}

IntegerVector lookup(IntegerVector tokens_, 
                     IntegerVector tokens_loc_, 
                     std::unordered_set<Key> &keys,
                     int span_max,
                     int id){
    
    std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
    IntegerVector tokens_loc = tokens_loc_;
    for(int span = 1; span <= span_max; span++){
        //Rcout << "Span " << span << "\n";
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Key tokens_sub(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = keys.find(tokens_sub) != keys.end();
            if(is_in){
                tokens_loc[i] = id;
            }
        }
    }
    return tokens_loc;
}


// [[Rcpp::export]]
List qatd_cpp_lookup_int_list(List texts_, 
                               List texts_loc_,
                               List keys,
                               int id){
    
    std::unordered_set<Key> set_keys;
    int span_max = 0;
    for(int g = 0; g < keys.size(); g++){
        if(has_na(keys[g])) continue;
        Key key = keys[g];
        set_keys.insert(key);
        if(span_max < key.size()) span_max = key.size();
    }
    //Rcout << "Span max " << span_max << "\n";
    
    List texts = texts_;
    List texts_loc = texts_loc_;
    
    for (int h = 0; h < texts.size(); h++){
        texts_loc[h] = lookup(texts[h], texts_loc[h], set_keys, span_max, id);
    }
    
    return(texts_loc);
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
toks_loc <- qatd_cpp_structcopy_int_list(toks)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- mapply(c, 1:1000, 1:1000*2, SIMPLIFY = FALSE)
#dict <- list(c(1, NA))
#dict <- as.list(c(1:1000))
qatd_cpp_lookup_int_list2(toks, toks_loc, dict, 99)

old <- function(x, tokens, seqs){
    for(i in 1:length(seqs)){
        tokens <- qatd_cpp_lookup_int_list2(x, tokens, seqs[[i]], 99)
    }
    return(tokens)
}
old(toks, toks_loc, dict)

microbenchmark::microbenchmark(
    qatd_cpp_lookup_int_list2(toks, toks_loc, dict, 99),
    old(toks, toks_loc, dict), times=1
)

*/
