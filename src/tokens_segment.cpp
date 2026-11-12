#include "lib.h"
//#include "dev.h"
using namespace quanteda;

// begin & end position of boundaries, pattern matches 
typedef std::pair<size_t, size_t> Bound;
typedef std::vector<Bound> Bounds;

// begin & end of new texts, begin & end of boundaries
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
   
    Bounds bounds;
    size_t i = (position == 1) ? 0 : min_size;
    size_t size = i + 1;
    while (i < tokens.size()) {
        for (std::size_t span : spans) {
            if (tokens.size() < i + span) continue;
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_patterns.find(ngram) != set_patterns.end();
            if (is_in) {
                Rcout << "pattern: " << i << " to " << (i + span) << "\n";
                bounds.push_back(std::make_pair(i, i + span));
                size = min_size - 1;
                i += min_size - 1;
                break;
            } 
        }
        if (size >= max_size) {
            Rcout << "size: " << i << " to " << i << "\n";
            bounds.push_back(std::make_pair(i, i));
            size = min_size - 1;
            i += min_size - 1;
        }
        size++;
        i++;
    }
    
    Segments segments;
    if (bounds.empty()) {
        segments.push_back(std::make_tuple(0, tokens.size(), 0, 0));
        N++;
        return segments;
    }
    
    // sort by the starting positions
    std::sort(bounds.begin(), bounds.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
        return left.first < right.first;
    });
    
    if (position == 1) {
        // preceded by delimiter
        if (0 < bounds.front().first) {
            segments.push_back(std::make_tuple(0, bounds.front().first, 0, 0)); // first segment
            Rcout << "segment first" << ": " << 0 << " " << bounds.front().first << "\n";
            N++;
        }
        for (size_t j = 0; j < bounds.size(); j++) {
            int from = bounds[j].first; // first token of the match
            if (remove) {
                from = bounds[j].second;
            }
            int to = tokens.size(); // the last token of the document
            if (j < bounds.size() - 1) {
                to = bounds[j + 1].first;
            }
            Rcout << "segment " << j << ": " << from << " " << to << "\n";
            segments.push_back(std::make_tuple(from, to, bounds[j].first, bounds[j].second));
            N++;
        }
        
    } else {
        
        // followed by delimiter
        for (size_t j = 0; j < bounds.size(); j++) {
            int from = 0; // the first token of the document
            if (j > 0) {
                from = bounds[j - 1].second;
            }
            int to = bounds[j].second; // last token of the match
            if (remove) {
                to = bounds[j].first;
            }
            Rcout << "segment " << j << ": " << from << " " << to << "\n";
            segments.push_back(std::make_tuple(from, to, bounds[j].first, bounds[j].second));
            N++;
        }
        if (bounds.back().second < tokens.size()) {
            segments.push_back(std::make_tuple(bounds.back().second, tokens.size(), 0, 0)); // last segment
            Rcout << "segment last" << ": " << bounds.back().second << " " << tokens.size() << "\n";
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
    std::vector<std::string> matches(N, "");
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Segments segments = temp[h];
        if (segments.empty()) continue;
        Text text = xptr->texts[h];
        for (size_t i = 0; i < segments.size(); i++) {
            Segment segment = segments[i];

            // extract text segments
            if (0 <= std::get<0>(segment) && std::get<1>(segment) <= (int)text.size()) {
                Text text_new(text.begin() + std::get<0>(segment), text.begin() + std::get<1>(segment));
                texts_new[j] = text_new;
            }
            
            // extract matched patterns
            if (remove && 0 <= std::get<2>(segment) && 0 <= std::get<3>(segment)) {
                Text match(text.begin() + std::get<2>(segment), text.begin() + std::get<3>(segment));
                matches[j] = join_strings(match, xptr->types, " ");
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
#toks <- tokens("a b . c c . d", xptr = TRUE)
toks <- tokens("a b", xptr = TRUE)
out <- cpp_tokens_segment(toks, list(c(3L)), FALSE, 2, min_size = 1, max_size = 100)
#out <- cpp_tokens_segment(toks, list(c(3L, 4L)), FALSE, 1, min_size = 1, max_size = 1)
quanteda:::cpp_as_list(out)
#attributes(out)

*/
