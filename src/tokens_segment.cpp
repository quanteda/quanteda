#include "lib.h"
//#include "dev.h"
using namespace quanteda;

// begin & end position of pattern matches
typedef std::pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

// begin & end of new texts, begin * end of pattern matches
typedef std::tuple<int, int, int, int> Segment;
typedef std::vector<Segment> Segments;

Segments segment(const Text &tokens,
                 UintParam &N,
                 const std::vector<std::size_t> &spans,
                 const SetNgrams &set_patterns,
                 const bool &remove,
                 const int &position,
                 const size_t &min_size,
                 const size_t &max_size){
    
    if(tokens.empty()) return {}; // return empty vector for empty text
   
    Targets targets;
    size_t size = 0;
    size_t i = min_size;
    while (i < tokens.size()) {
        size++;
        for (std::size_t span : spans) {
            if (tokens.size() < span) continue;
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_patterns.find(ngram) != set_patterns.end();
            if (is_in || size >= max_size) {
                targets.push_back(std::make_pair(i, i + span - 1));
                Rcout << "target: " << i << " to " << (i + span - 1) << "\n";
                size = 0;
                i += min_size - 1;
                break;
            }
        }
        i++;
    }

    // Targets targets;
    // for (std::size_t span : spans) { // substitution starts from the longest sequences
    //     if (tokens.size() < span) continue;
    //     for (size_t i = 0; i < tokens.size() - (span - 1); i++) {// start from previous segmentation point?
    //         Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
    //         bool is_in = set_patterns.find(ngram) != set_patterns.end();
    //         if (is_in) {
    //             targets.push_back(std::make_pair(i, i + span - 1));
    //         }
    //     }
    // }
    
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
 * @param min_size minimum size of segments
 * @param max_size maximum size of segments
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_segment(TokensPtr xptr,
                             const List &patterns_,
                             const bool &remove,
                             const int &position,
                             const int min_size = 1, 
                             const int max_size = 10000,
                             const int thread = -1) {
    
    SetNgrams set_patterns;
    std::vector<std::size_t> spans = register_ngrams(patterns_, set_patterns);
    UintParam N(0);
    
    // dev::Timer timer;
    // dev::start_timer("Dictionary detect", timer);
    std::size_t H = xptr->texts.size();
    std::vector<Segments> temp(H);
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                temp[h] = segment(xptr->texts[h], N, spans, set_patterns, remove, position, min_size, max_size);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        temp[h] = segment(xptr->texts[h], N, spans, set_patterns, remove, position, min_size, max_size);
    }
#endif
    
    Texts texts_new(N);
    std::vector<int> index(N);
    std::vector<std::string> matches(N);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Segments segments = temp[h];
        if (segments.empty()) continue;
        Text text = xptr->texts[h];
        for (size_t i = 0; i < segments.size(); i++) {
            Segment segment = segments[i];
            
            // extract text segments
            if (0 <= std::get<0>(segment) && std::get<1>(segment) < (int)text.size()) {
                Text text_new(text.begin() + std::get<0>(segment), text.begin() + std::get<1>(segment) + 1);
                texts_new[j] = text_new;
            } else {
                texts_new[j] = {};
            }
            
            // extract matched patterns
            if (remove && 0 <= std::get<2>(segment) && 0 <= std::get<3>(segment)) {
                Text match(text.begin() + std::get<2>(segment), text.begin() + std::get<3>(segment) + 1);
                matches[j] = join_strings(match, xptr->types, " ");
            } else {
                matches[j] = "";
            }
            index[j] = (int)h + 1;
            j++;
        }
    }
    
    TokensObj *ptr_new = new TokensObj(texts_new, xptr->types, xptr->recompiled);
    TokensPtr xptr_new = TokensPtr(ptr_new, true);
    
    xptr_new.attr("matches") = encode(matches);
    xptr_new.attr("index") = Rcpp::wrap(index);
    
    return xptr_new;
}




/***R
library(quanteda)
toks <- tokens("a b b . c c c c c . d", xptr = TRUE)
out <- cpp_tokens_segment(toks, list(3L), TRUE, 2, min_size = 2, max_size = 5)
quanteda:::cpp_as_list(out)
attributes(out)

*/
