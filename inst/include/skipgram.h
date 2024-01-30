#include "lib.h"
#include "dev.h"
using namespace quanteda;


inline unsigned int ngram_id(const Ngram &ngram,
                             MapNgrams &map_ngram,
                             IdNgram &id_ngram){
    
    auto it1 = map_ngram.find(ngram);
    if (it1 != map_ngram.end()) return it1->second;
    auto it2 = map_ngram.insert(std::pair<Ngram, unsigned int>(ngram, id_ngram.fetch_add(1, std::memory_order_relaxed)));
    return it2.first->second;
    
}

inline void skip(const Text &tokens,
                 Text &tokens_ng,
                 const SetNgrams &set_words,
                 const unsigned int &start,
                 const unsigned int &n, 
                 const std::vector<unsigned int> &skips,
                 Ngram ngram,
                 MapNgrams &map_ngram,
                 IdNgram &id_ngram) {
    
    //Rcout << "Ngram Size " << ngram.size() << ", ";
    //Rcout << "Position " << start << "\n";
    
    ngram.push_back(tokens[start]);
    
    // Rcout << "Size " << tokens.size() << ", ";
    // Rcout << "Token " << tokens[start] << ", ";
    // Rcout << "N " << n << "\n";
    
    if (ngram.size() < n) {
        for (std::size_t j = 0; j < skips.size(); j++) {
            unsigned int next = start + 1 + skips[j];
            if(tokens.size() - 1 < next) break;
            if(tokens[next] == 0) break; // skip padding
            // Rcout << "Compound " << tokens[start] <<  " and " << tokens[next] << " at " << start << "\n";
            skip(tokens, tokens_ng, set_words, next, n, skips, ngram, map_ngram, id_ngram);
        }
    } else {
        // dev::print_ngram(ngram);
        // Rcout << "Ngram ID " << ngram_id(ngram, map_ngram, id_ngram) << "\n";
        if (set_words.size() > 0) { // for compounding
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                // Rcout << "Save ngram ";
                // dev::print_ngram(ngram);
                tokens_ng.push_back(ngram_id(ngram, map_ngram, id_ngram));
            }
        } else {
            tokens_ng.push_back(ngram_id(ngram, map_ngram, id_ngram));
        }
    }
}
