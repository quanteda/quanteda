//#include "dev.h"
#include "quanteda.h"
#include "recompile.h"
using namespace quanteda;

Text match(Text tokens, 
           const std::vector<std::size_t> &spans,
           const bool &overlap,
           const MapNgrams &map_words){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false);
    std::size_t count_match = 0;
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            if (!overlap && flags_match[i]) continue; // ignore matched tokens 
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_words.find(ngram);
            if (it != map_words.end()) {
                //Rcout << it->second << "\n";
                std::fill(flags_match.begin() + i, flags_match.begin() + i + span, true); // mark tokens matched
                tokens_multi[i].push_back(it->second); // keep multiple keys in the same position
                count_match++;
            }
        }
    }
    
    if (count_match == 0) return tokens; // return original tokens if no match
    
    // Add original tokens that did not match
    for (std::size_t i = 0; i < tokens.size(); i++) {
        if (!flags_match[i]) {
            tokens_multi[i].push_back(tokens[i]); 
            count_match++;
        }
    }
    
    // Flatten the vector of vector
    Text tokens_flat;
    tokens_flat.reserve(count_match);
    for (auto &tokens_sub: tokens_multi) {
        if (overlap) {
            tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.end());
        } else {
            if (tokens_sub.size() > 0) {
                tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.begin() + 1);
            }
        }
    }
    return tokens_flat;
}

struct match_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const bool &overlap;
    const MapNgrams &map_words;
    
    // Constructor
    match_mt(Texts &texts_, const std::vector<std::size_t> &spans_, const bool &overlap_, MapNgrams &map_words_):
             texts(texts_), spans(spans_), overlap(overlap_), map_words(map_words_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            texts[h] = match(texts[h], spans, overlap, map_words);
        }
    }
};

/* 
* This funciton substitutes features in tokens object with new IDs. 
* The number of threads is set by RcppParallel::setThreadOptions()
* @used tokens_compound()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param words_ list of features to substitute
* @param ids_ IDs to be placed after substitution
* @param overlap overlapped features are detected
* 
*/

// [[Rcpp::export]]
List qatd_cpp_tokens_match(const List &texts_,
                           const CharacterVector types_,
                           const List &words_,
                           const IntegerVector &ids_,
                           const bool &overlap){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);

    MapNgrams map_words;
    std::vector<std::size_t> spans = register_ngrams(words_, ids_, map_words);

    // dev::Timer timer;
    // dev::start_timer("Token match", timer);
#if QUANTEDA_USE_TBB
    match_mt match_mt(texts, spans, overlap, map_words);
    parallelFor(0, texts.size(), match_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        texts[h] = match(texts[h], spans, overlap, map_words);
    }
#endif
    // dev::stop_timer("Token match", timer);
    return recompile(texts, types);
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
dict <- list(c(1, 2), c(1, 2, 3))
id <- rep(1, length(dict)) * 100
qatd_cpp_tokens_match(toks, dict, id, FALSE)
qatd_cpp_tokens_match(toks, dict, id, TRUE)



*/
