#include <Rcpp.h>
#include <vector>
#include <algorithm>  
// [[Rcpp::plugins(cpp11)]]
#include <unordered_map>
#include "quanteda.h"
#include "dev.h"

using namespace Rcpp;
using namespace std;
using namespace quanteda;


typedef std::vector<unsigned int> Key;

namespace std {
template <>

// Custom hash function for key objects
struct hash<Key> {
    std::size_t operator()(const Key &vec) const {
        unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
        return std::hash<unsigned int>()(seed);
    }
};
}

IntegerVector replace(IntegerVector tokens_, 
                     std::unordered_map<Key, int> &seqs,
                     int span_max,
                     int &id_new){
    
    std::vector<int> tokens = Rcpp::as< std::vector<int> >(tokens_);
    bool match = false;
    for(int span = span_max; span >= 1; span--){ // substitution starts from the longest sequences
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Key tokens_sub(tokens.begin() + i, tokens.begin() + i + span);
            int &id = seqs[tokens_sub];
            if(id){
                match = true;
                if(id < 0){
                    id = id_new++;
                }
                std::fill(tokens.begin() + i + 1, tokens.begin() + i + span, -1); // -1 are place holders
                tokens[i] = id;
            }
        }
    }
    if(match) tokens.erase(std::remove(tokens.begin(), tokens.end(), -1), tokens.end());
    return wrap(tokens);
}


/*
 * This function is used in joinTokens2 to substitutes sequences of IDs with new IDs.
 * @author Kohei Watanabe
 * @param texts_ tokens object to be modified
 * @param seqs_ a list of integer vectors for sequences
 * @param ids new IDs to be substibuted with. Note length(seqs_) == length(ids)
 * @param id_start first newly generated IDs
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_replace_int_list(List texts_, 
                               List seqs,
                               IntegerVector ids,
                               int id_start){
    
    std::unordered_map<Key, int> map_seqs;
    int span_max = 0;
    for(int g = 0; g < seqs.size(); g++){
        if(has_na(seqs[g])) continue;
        Key key = seqs[g];
        if(ids[g] == NA_INTEGER){
            //Rcout << "ID is NA\n";
            map_seqs.insert({key, -1});
        }else{
            //Rcout << "ID is " << ids[g] << "\n";
            map_seqs.insert({key, ids[g]});
        }
        if(span_max < key.size()) span_max = key.size();
    }
    
    List texts = clone(texts_);
    int id_new = id_start;
    for (int h = 0; h < texts.size(); h++){
        texts[h] = replace(texts[h], map_seqs, span_max, id_new);
    }
    
    // Extract new IDs from unordered_map
    IntegerVector ids_new(seqs.size());
    for(int j = 0; j < seqs.size(); j++){
        if(has_na(seqs[j])) continue;
        Key key = seqs[j];
        ids_new[j] = map_seqs[key];
    }
    
    return Rcpp::List::create(Rcpp::Named("text") = texts,
                              Rcpp::Named("id_new") = ids_new);
}

/***R

# txt <- c("a b c d e f g", "A B C D E F G", "A b C d E f G", 
#       "aaa bbb ccc ddd eee fff ggg", "a_b b_c c_d d_e e_f f_g") 
# toks <- tokens(txt)
# seqs <- tokens(c("a b", "c d", "aaa bbb", "eEE FFf", "d_e e_f", "z z"), 
#             hash = FALSE, what = "fastestword")
# seqs_ids <- lapply(seqs, function(x, y) match(x, y), attr(toks, 'types'))
# types_new <- sapply(seqs, paste0, collapse = '_')
# ids <- match(types_new, attr(toks, 'types'))
# res <- qatd_cpp_replace_int_list(toks, seqs_ids, ids, 100)
# 
# res$text
# res$id_new
# 
# microbenchmark::microbenchmark(
#     qatd_cpp_replace_int_list(toks, seqs_ids, ids, 100)
# )

*/
