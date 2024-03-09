#include "lib.h"
#include "skipgram.h"
//#include "dev.h"
using namespace quanteda;

Text skipgram(const Text &tokens,
              const std::vector<unsigned int> &ns, 
              const std::vector<unsigned int> &skips,
              MapNgrams &map_ngram,
              IdNgram &id_ngram) {
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    // Pre-allocate memory
    int size_reserve = 0;
    for (std::size_t k = 0; k < ns.size(); k++) {
        size_reserve += std::pow(skips.size(), ns[k]) * tokens.size();
    }
    Text tokens_ng;
    tokens_ng.reserve(size_reserve);
    SetNgrams set_words; // keep empty
    
    // Generate skipgrams recursively
    for (std::size_t k = 0; k < ns.size(); k++) {
        unsigned int n = ns[k];
        if (tokens.size() < n) continue;
        Ngram ngram;
        ngram.reserve(n);
        for (std::size_t start = 0; start < tokens.size() - (n - 1); start++) {
            if(tokens[start] == 0) continue; // skip padding
            skip(tokens, tokens_ng, set_words, start, n, skips, ngram, map_ngram, id_ngram); // Get ngrams as reference
        }
    }
    return tokens_ng;
}

/* 
* Function to generates ngrams/skipgrams
* @used tokens_ngrams()
* @creator Kohei Watanabe
* @param delim_ string to join words
* @param ns_ size of ngramss
* @param skips_ size of skip (this has to be 1 for ngrams)
* 
*/

// [[Rcpp::export]]
TokensPtr cpp_tokens_ngrams(TokensPtr xptr,
                            const String delim_,
                            const IntegerVector ns_,
                            const IntegerVector skips_,
                            const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = xptr->types;
    std::string delim = delim_;
    std::vector<unsigned int> ns = Rcpp::as< std::vector<unsigned int> >(ns_);
    std::vector<unsigned int> skips = Rcpp::as< std::vector<unsigned int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    MapNgrams map_ngram;
    map_ngram.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);
    
    //dev::Timer timer;
    //dev::start_timer("Ngram generation", timer);
    std::size_t H = texts.size();
    IdNgram id_ngram(1);
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                texts[h] = skipgram(texts[h], ns, skips, map_ngram, id_ngram);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        texts[h] = skipgram(texts[h], ns, skips, map_ngram, id_ngram);
    }
#endif
    //dev::stop_timer("Ngram generation", timer);
    
    // Extract only keys in order of the id
    VecNgrams keys_ngram(id_ngram - 1);
    for (std::pair<Ngram, unsigned int> it : map_ngram) {
        keys_ngram[it.second - 1] = it.first;
    }
    
    //dev::start_timer("Token generation", timer);
    // Create ngram types
    std::size_t I = keys_ngram.size();
    Types types_new(I);
#if QUANTEDA_USE_TBB
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, I), [&](tbb::blocked_range<int> r) {
          for (int i = r.begin(); i < r.end(); ++i) {
              types_new[i] = join_strings(keys_ngram[i], types, delim);
          }    
        });
    });
#else
    for (std::size_t i = 0; i < I; i++) {
        types_new[i] = join_strings(keys_ngram[i], types, delim);
    }
#endif
    
    xptr->texts = texts;
    xptr->types = types_new;
    xptr->recompiled = false;
    return xptr;

}




/*** R

library(quanteda)
#txt <- c('a b c d e')
txt <- c('a b c d e', 'c d e f g')
xtoks <- quanteda::tokens(txt, xptr = TRUE)
xtoks_ng <- cpp_tokens_ngrams(xtoks, "-", 2, 1)
as.list(xtoks_ng)



*/

