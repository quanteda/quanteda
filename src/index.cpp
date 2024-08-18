#include "lib.h"
//#include "dev.h"
using namespace quanteda;

typedef std::tuple<unsigned int, int, int, int> Match;
#if QUANTEDA_USE_TBB
typedef tbb::concurrent_vector<Match> Matches;
#else
typedef std::vector<Match> Matches;
#endif

void index(Text tokens,
           const int document,
           const std::vector<std::size_t> &spans,
           const MultiMapNgrams &map_pats,
           Matches &matches) {
    
    if (tokens.empty()) return;
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_pats.equal_range(ngram);
            for (auto it = range.first; it != range.second; ++it) {
                unsigned int pat = it->second;
                Match match = std::make_tuple(pat, document, i, i + span - 1);
                matches.push_back(match);
            }
        }
    }
}

/* 
 * Function to locates patterns in tokens 
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
    Ngrams words = to_ngrams(words_);

    std::size_t G = words.size();
    std::vector<std::size_t> spans(G);
    for (std::size_t g = 0; g < G; g++) {
        Ngram value = words[g];
        map_pats.insert(std::make_pair(value, g));
        spans[g] = value.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    //dev::Timer timer;
    Matches matches;
    
    //dev::start_timer("Search keywords", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                index(texts[h], h, spans, map_pats, matches);
            }
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        index(texts[h], h, spans, map_pats, matches);
    }
#endif
    //dev::stop_timer("Search keywords", timer);
    
    std::size_t I = matches.size();
    IntegerVector pos_from_(I), pos_to_(I);
    IntegerVector patterns_(I), documents_(I);
    for (std::size_t i = 0; i < I; i++) {
        Match match = matches[i];
        patterns_[i] = std::get<0>(match) + 1;
        documents_[i] = std::get<1>(match) + 1;
        pos_from_[i] = std::get<2>(match) + 1;
        pos_to_[i] = std::get<3>(match) + 1;
    }
    
    return DataFrame::create(_["docname"] = documents_,
                             _["from"]    = pos_from_,
                             _["to"]      = pos_to_,
                             _["pattern"] = patterns_,
                             _["stringsAsFactors"] = false);
}




/***R

toks <- tokens(c("a b c d e", "e f g"), xptr = TRUE)
cpp_index(toks, list(5))
cpp_index(toks, list(c(3, 4), 7))


*/
