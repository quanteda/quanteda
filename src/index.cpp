#include "lib.h"
//#include "dev.h"
using namespace quanteda;

typedef std::tuple<unsigned int, size_t, size_t> Match;
typedef std::vector<Match> Matches;

Matches index(Text tokens,
                   const std::vector<std::size_t> &spans,
                   const MultiMapNgrams &map_pats,
                   UintParam &N){
    
    if(tokens.empty()) return {}; // return empty vector for empty text
    
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
                N++;
            }
        }
    }
    
    // sort by the starting positions
    //std::sort(matches.begin(), matches.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
    //   return left.first < right.first;
    //});
    return matches;
}

/* 
 * Function to locates tokens for kwic() and index() 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used index()
 * @creator Kohei Watanabe
 * @param words_ list of target features
 */

// [[Rcpp::export]]
DataFrame cpp_index(TokensPtr xptr,
                    const List &words_,
                    const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = xptr->types;

    MultiMapNgrams map_pats;
    map_pats.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams words = Rcpp::as<Ngrams>(words_);
    std::size_t G = words.size();
    
    std::vector<unsigned int> pats(G);
    unsigned int p = 0;
    for (std::size_t g = 0; g < G; g++) {
        pats[g] = p++;
    }
    
    std::vector<std::size_t> spans(G);
    for (std::size_t g = 0; g < G; g++) {
        Ngram value = words[g];
        unsigned int pat = pats[g];
        map_pats.insert(std::pair<Ngram, unsigned int>(value, pat));
        spans[g] = value.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    //dev::Timer timer;
    std::vector<Matches> temp(texts.size());
    
    //dev::start_timer("Search keywords", timer);
    UintParam N(0);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                temp[h] = index(texts[h], spans, map_pats, N);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        temp[h] = index(texts[h], spans, map_pats, N);
    }
#endif
    //dev::stop_timer("Search keywords", timer);
    
    //dev::start_timer("Create strings", timer);
    IntegerVector pos_from_(N), pos_to_(N);
    IntegerVector patterns_(N), documents_(N);

    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Matches matches = temp[h];
        if (matches.empty()) continue;
        Text tokens = texts[h];
        for (size_t i = 0; i < matches.size(); i++) {
            Match match = matches[i];
            patterns_[j] = std::get<0>(match) + 1;
            pos_from_[j] = std::get<1>(match) + 1;
            pos_to_[j] = std::get<2>(match) + 1;
            documents_[j] = h + 1;
            j++;
        }
    }
    return DataFrame::create(_["docname"] = documents_,
                             _["from"]    = pos_from_,
                             _["to"]      = pos_to_,
                             _["pattern"] = patterns_,
                             _["stringsAsFactors"] = false);
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#cpp_tokens_contexts(toks, dict, 2)
cpp_index(toks, letters, list(10), 1, "_")
cpp_index(toks, letters, list(10), 1, "_")
cpp_index(toks, letters, list(c(3, 4), 7), 2, "_")
cpp_index(toks, letters, list(c(3, 4), 7), 2, "_")
cpp_index(toks, letters, c(3, 4, 7), 2, "_")
cpp_index(toks, letters, c(3, 4, 7), 2, "_")


*/
