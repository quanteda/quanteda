#include "lib.h"
#include "recompile.h"
#include "dev.h"
using namespace quanteda;

typedef std::tuple<unsigned int, size_t, size_t> Match;
typedef std::vector<Match> Matches;

Matches index(Text tokens,
                   const std::vector<std::size_t> &spans,
                   const MultiMapNgrams &map_pats,
                   UintParam &n_match){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Matches matches;
    matches.reserve(tokens.size());
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_pats.equal_range(ngram);
            for (auto it = range.first; it != range.second; ++it) {
                unsigned int pat = it->second;
                matches.push_back(std::make_tuple(pat, i, i + span - 1));
                n_match++;
            }
        }
    }
    
    // sort by the starting positions
    //std::sort(matches.begin(), matches.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
    //   return left.first < right.first;
    //});
    return matches;
}

struct index_mt : public Worker{
    
    Texts &texts;
    std::vector<Matches> &temp;
    const std::vector<std::size_t> &spans;
    const MultiMapNgrams &map_pats;
    UintParam &n_match;
    
    // Constructor
    index_mt(Texts &texts_, std::vector<Matches> &temp_,
            const std::vector<std::size_t> &spans_, const MultiMapNgrams &map_pats_,
            UintParam &n_match_):
            texts(texts_), temp(temp_), spans(spans_), map_pats(map_pats_), 
            n_match(n_match_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            temp[h] = index(texts[h], spans, map_pats, n_match);
        }
    }
};


/* 
 * This function generate generates keyword-in-contexts. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used index()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param words_ list of target features
 * @param pats_ index for patterns
 * @param delim_ character to join tokens
 */

// [[Rcpp::export]]
DataFrame qatd_cpp_index(const List &texts_,
                        const CharacterVector types_,
                        const List &words_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector docnames_ = texts_.attr("names");
    CharacterVector patters_ = words_.attr("names");

    MultiMapNgrams map_pats;
    map_pats.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams words = Rcpp::as<Ngrams>(words_);
    std::vector<unsigned int> pats(words_.size());
    unsigned int p = 0;
    for (size_t f = 0; f < pats.size(); f++) {
        pats[f] = p++;
    }
    
    size_t len = words.size();
    std::vector<std::size_t> spans(len);
    for (size_t g = 0; g < len; g++) {
        Ngram value = words[g];
        unsigned int pat = pats[g];
        map_pats.insert(std::pair<Ngram, unsigned int>(value, pat));
        spans[g] = value.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    dev::Timer timer;
    std::vector<Matches> temp(texts.size());
    
    //dev::start_timer("Search keywords", timer);
    UintParam n_match = 0;
#if QUANTEDA_USE_TBB
    index_mt index_mt(texts, temp, spans, map_pats, n_match);
    parallelFor(0, texts.size(), index_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = index(texts[h], spans, map_pats, n_match);
    }
#endif
    //dev::stop_timer("Search keywords", timer);
    
    //dev::start_timer("Create strings", timer);
    IntegerVector kw_from_(n_match), kw_to_(n_match);
    CharacterVector kw_pattern_(n_match), kw_docname_(n_match);

    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Matches matches = temp[h];
        if (matches.size() == 0) continue;
        Text tokens = texts[h];
        for (size_t i = 0; i < matches.size(); i++) {
            Match match = matches[i];
            kw_pattern_[j] = patters_[std::get<0>(match)];
            kw_from_[j] = std::get<1>(match) + 1;
            kw_to_[j] = std::get<2>(match) + 1;
            kw_docname_[j] = docnames_[h];
            j++;
        }
    }
    return DataFrame::create(_["docname"] = kw_docname_,
                             _["from"]    = kw_from_,
                             _["to"]      = kw_to_,
                             _["pattern"] = kw_pattern_,
                             _["stringsAsFactors"] = false);
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_index(toks, letters, list(10), 1, "_")
qatd_cpp_index(toks, letters, list(10), 1, "_")
qatd_cpp_index(toks, letters, list(c(3, 4), 7), 2, "_")
qatd_cpp_index(toks, letters, list(c(3, 4), 7), 2, "_")
qatd_cpp_index(toks, letters, c(3, 4, 7), 2, "_")
qatd_cpp_index(toks, letters, c(3, 4, 7), 2, "_")


*/
