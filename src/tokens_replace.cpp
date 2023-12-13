#include "lib.h"
//#include "dev.h"

using namespace quanteda;

Text replace(Text tokens, 
             const std::vector<std::size_t> &spans,
             MapNgrams &map_pat,
             Ngrams &ids_repls,
             const int offset){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false); // flag matched tokens
    std::size_t match = 0;
    bool none = true;
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        //Rcout << "span:" << span << "\n";
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_pat.find(ngram);
            if (it != map_pat.end()) {
                std::size_t j = i;
                if (offset > 0) {
                    j = std::min((int)i + offset + (int)span - 1, (int)tokens.size() - 1);
                } else if (0 < offset) {
                    j = std::max(0, (int)i - offset);
                } else {
                    std::fill(flags_match.begin() + j, flags_match.begin() + j + span, true); // mark tokens matched
                }
                tokens_multi[j].insert(tokens_multi[j].end(), ids_repls[it->second].begin(), ids_repls[it->second].end());
                match += ids_repls[it->second].size();
                none = false;
            }
        }
    }
    
    // return original tokens if no match
    if (none) return tokens; 
    
    // Add original tokens that did not match
    for (std::size_t i = 0; i < tokens.size(); i++) {
        if (!flags_match[i]) {
            tokens_multi[i].push_back(tokens[i]); 
            match++;
            //Rcout << tokens[i] << "\n";
        }
    }
    
    // Flatten the vector of vector
    Text tokens_flat;
    tokens_flat.reserve(match);
    for (auto &tokens_sub: tokens_multi) {
        if (!tokens_sub.empty()) {
            tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.end());
        }
        //dev::print_ngram(tokens_sub);
    }
    return tokens_flat;
}

/* 
* Function to replace tokens
* @used tokens_replace()
* @creator Kohei Watanabe
* @param patterns_ IDs of patterns
* @param replacements_ IDs to replace patterns. Must be the same length as patterns_
*/


// [[Rcpp::export]]
TokensPtr cpp_tokens_replace(TokensPtr xptr,
                             const List &patterns_,
                             const List &replacements_,
                             const int offset = 0,
                             const int thread = -1) {
    
    Texts texts = xptr->texts;
    Ngrams ids_repls = Rcpp::as<Ngrams>(replacements_);
    //dev::Timer timer;
    //dev::start_timer("Map construction", timer);

    MapNgrams map_pat;
    map_pat.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams pats = Rcpp::as<Ngrams>(patterns_);

    size_t len = std::min(pats.size(), ids_repls.size());
    std::vector<std::size_t> spans(len);
    for (size_t g = 0; g < len; g++) {
        Ngram pat = pats[g];
        map_pat.insert(std::pair<Ngram, unsigned int>(pat, g));
        spans[g] = pat.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    //dev::stop_timer("Map construction", timer);
    
    //dev::start_timer("Pattern replace", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                texts[h] = replace(texts[h], spans, map_pat, ids_repls, offset);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        texts[h] = replace(texts[h], spans, map_pat, ids_repls, offset);
    }
#endif
    xptr->texts = texts;
    xptr->recompiled = false;
    return xptr;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#toks <- list(rep(1:10, 1))
#from <- list(c(1, 2), c(2, 3), c(5, 6), 10, 20)
#to <- list(c(1, 1, 1), c(2, 2, 2), c(5, 5, 5), 1, 2)
from <- list(c(9, 10))
to <- list(0)

#cpp_tokens_replace(toks, letters, dict, integer(0), 0)
cpp_tokens_replace(toks, letters, from, to)
#cpp_tokens_replace(toks, letters, from, to)

*/
