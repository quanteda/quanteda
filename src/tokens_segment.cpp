#include "lib.h"
//#include "dev.h"
using namespace quanteda;

typedef std::pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

typedef std::tuple<int, int, int, int> Segment;
typedef std::vector<Segment> Segments;

Segments segment(Text tokens,
                 UintParam &N,
                const std::vector<std::size_t> &spans,
                const SetNgrams &set_patterns,
                const bool &remove,
                const int &position){
    
    if(tokens.empty()) return {}; // return empty vector for empty text
    
    Targets targets;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_patterns.find(ngram) != set_patterns.end();
            if (is_in) {
                targets.push_back(std::make_pair(i, i + span - 1));
            }
        }
    }
    
    Segments segments;
    if (targets.empty()) {
        segments.push_back(std::make_tuple(0, tokens.size() - 1, -1, -1));
        N++;
        return segments;
    }
    
    // sort by the starting positions
    std::sort(targets.begin(), targets.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
        return left.first < right.first;
    });
    
    if (position == 1) {
        // preceeded by delimiter
        if (0 < targets.front().first) {
            segments.push_back(std::make_tuple(0, targets.front().first - 1, -1, -1));
            N++;
        }
        for (size_t i = 0; i < targets.size(); i++) {
            int from = targets[i].first;
            if (remove) {
                from = targets[i].second + 1;
            }
            int to = tokens.size() - 1;
            if (i < targets.size() - 1) {
                to = targets[i + 1].first - 1;
            }
            segments.push_back(std::make_tuple(from, to, targets[i].first, targets[i].second));
            N++;
            //Rcout << "segment " << i << ": " << from << " " << to << "\n";
        }
        
    } else {
        
        // followed by delimiter
        for (size_t i = 0; i < targets.size(); i++) {
            int from = 0;
            if (i > 0) {
                from = targets[i - 1].second + 1;
            }
            int to = targets[i].second;
            if (remove) {
                to = targets[i].first - 1;
            }
            segments.push_back(std::make_tuple(from, to, targets[i].first, targets[i].second));
            N++;
            //Rcout << "segment " << i << ": " << from << " " << to << "\n";
        }
        if (targets.back().second < tokens.size() - 1) {
            segments.push_back(std::make_tuple(targets.back().second + 1, tokens.size() - 1, -1, -1));
            N++;
        }
    }
    return segments;
}


/* 
 * Function to segment documents
 * @used tokens_segment()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param patterns_ list of target patterns
 * @param remove remove and save matched patterns if true
 * @param position determine position to split texts 1=before; 2=after
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_segment(TokensPtr xptr,
                             const List &patterns_,
                             const bool &remove,
                             const int &position,
                             const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = xptr->types;
    UintParam N(0);
    SetNgrams set_patterns;
    std::vector<std::size_t> spans = register_ngrams(patterns_, set_patterns);
    
    // dev::Timer timer;

    // dev::start_timer("Dictionary detect", timer);
    std::size_t H = texts.size();
    std::vector<Segments> temp(H);
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                temp[h] = segment(texts[h], N, spans, set_patterns, remove, position);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = segment(texts[h], N, spans, set_patterns, remove, position);
    }
#endif
    
    Texts segments(N);
    std::vector<int> index(N);
    std::vector<std::string> matches(N);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Segments targets = temp[h];
        if (targets.empty()) continue;
        Text tokens = texts[h];
        for (size_t i = 0; i < targets.size(); i++) {
            Segment target = targets[i];
            
            // extract text segments
            if (0 <= std::get<0>(target) && std::get<1>(target) < (int)tokens.size()) {
                Text segment(tokens.begin() + std::get<0>(target), tokens.begin() + std::get<1>(target) + 1);
                segments[j] = segment;
            } else {
                segments[j] = {};
            }
            
            // extract matched patterns
            if (remove && 0 <= std::get<2>(target) && 0 <= std::get<3>(target)) {
                Text match(tokens.begin() + std::get<2>(target), tokens.begin() + std::get<3>(target) + 1);
                matches[j] = join_strings(match, types, " ");
            } else {
                matches[j] = "";
            }
            index[j] = (int)h + 1;
            j++;
        }
    }
    
  
    TokensObj *ptr_new = new TokensObj(segments, xptr->types, xptr->recompiled);
    TokensPtr xptr_new = TokensPtr(ptr_new, true);
    
    xptr_new.attr("matches") = encode(matches);
    xptr_new.attr("index") = Rcpp::wrap(index);
    
    return xptr_new;
}




/***R

toks <- tokens("aa bb cc")
xtoks <- as.tokens_xptr(toks)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#cpp_tokens_contexts(toks, dict, 2)
#cpp_tokens_segment(toks, letters, list(c(5, 6), 10), TRUE, 2)
out <- cpp_tokens_segment(xtoks, list(2), FALSE, 1)
attributes(out)
#cpp_tokens_segment(toks, letters, list(c(5, 6), 10), FALSE, 2)

# 
# cpp_tokens_segment(toks, letters, list(10))
# cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# cpp_tokens_segment(toks, letters, c(3, 4, 7))
# cpp_tokens_segment(toks, letters, c(3, 4, 7))


*/
