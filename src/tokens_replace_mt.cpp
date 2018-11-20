#include "quanteda.h"
#include "recompile.h"
#include "dev.h"
using namespace quanteda;

Text replace(Text tokens, 
            const std::vector<std::size_t> &spans,
            const MapNgrams &map_pat,
            const Texts &repls){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false); // flag matched tokens
    
    std::size_t match = 0;
    std::vector< std::vector<unsigned int> > keys(tokens.size()); 
    for (std::size_t span : spans) {
        if (tokens.size() < span) continue;
        //Rcout << "span:" << span << "\n";
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_pat.find(ngram);
            if (it != map_pat.end()) {
                //Rcout << "index:" << i << "\n";
                std::fill(flags_match.begin() + i, flags_match.begin() + i + span, true); // mark tokens matched
                //dev::print_ngram(repls[it->second]);
                tokens_multi[i].insert(tokens_multi[i].end(), repls[it->second].begin(), repls[it->second].end());
                //dev::print_ngram(tokens_multi[i]);
                match++;
            }
        }
    }
    
    if (match == 0) return tokens; // return original tokens if no match
    
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
        if (tokens_sub.size() > 0) {
            tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.end());
        }
        //dev::print_ngram(tokens_sub);
    }
    return tokens_flat;
}


struct replace_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const MapNgrams &map_pat;
    const Texts &repls;
    
    // Constructor
    replace_mt(Texts &texts_, const std::vector<std::size_t> &spans_, const MapNgrams &map_pat_, const Texts &repls_):
               texts(texts_), spans(spans_), map_pat(map_pat_), repls(repls_){}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            texts[h] = replace(texts[h], spans, map_pat, repls);
        }
    }
};

/* 
* This function replace patterns in tokens object.
* @used tokens_replace()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types of tokens
* @param patterns_ IDs of patterns
* @param replacements_ IDs to replace patterns. Must be the same length as patterns_
*/


// [[Rcpp::export]]
List qatd_cpp_tokens_replace(const List &texts_,
                            const CharacterVector types_,
                            const List &patterns_,
                            const List &replacements_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    Texts repls = Rcpp::as<Texts>(replacements_);
    
    //dev::Timer timer;
    //dev::start_timer("Map construction", timer);

    MapNgrams map_pat;
    map_pat.max_load_factor(GLOBAL_PATTERNS_MAX_LOAD_FACTOR);
    Ngrams pats = Rcpp::as<Ngrams>(patterns_);
    std::vector<std::size_t> spans(pats.size());
    for (size_t g = 0; g < std::min(pats.size(), repls.size()); g++) {
        Ngram pat = pats[g];
        map_pat.insert(std::pair<Ngram, unsigned int>(pat, g));
        spans[g] = pat.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    
    //dev::stop_timer("Map construction", timer);
    
    //dev::start_timer("Pattern replace", timer);
#if QUANTEDA_USE_TBB
    replace_mt replace_mt(texts, spans, map_pat, repls);
    parallelFor(0, texts.size(), replace_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        texts[h] = replace(texts[h], spans, id_max, map_pat);
    }
#endif
    //dev::stop_timer("Pattern replace", timer);
    return recompile(texts, types, true, true, is_encoded(types_));
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#toks <- list(rep(1:10, 1))
from <- list(c(1, 2), c(2, 3), c(5, 6), 10, 20)
to <- list(c(1, 1, 1), c(2, 2, 2), c(5, 5, 5), 1, 2)

#qatd_cpp_tokens_replace(toks, letters, dict, integer(0), 0)
qatd_cpp_tokens_replace(toks, letters, from, to)
#qatd_cpp_tokens_replace(toks, letters, from, to)

*/
