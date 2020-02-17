#include "lib.h"
#include "recompile.h"
//#include "dev.h"
using namespace quanteda;

typedef std::pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

typedef std::tuple<int, int, int, int> Segment;
typedef std::vector<Segment> Segments;

Segments segment(Text tokens,
                const std::vector<std::size_t> &spans,
                const SetNgrams &set_patterns,
                const bool &remove,
                const int &position){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
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
    if (targets.size() == 0) {
        segments.push_back(std::make_tuple(0, tokens.size() - 1, -1, -1));
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
            //Rcout << "segment " << i << ": " << from << " " << to << "\n";
        }
        if (targets.back().second < tokens.size() - 1) {
            segments.push_back(std::make_tuple(targets.back().second + 1, tokens.size() - 1, -1, -1));
        }
    }
    
    return segments;
}

struct segment_mt : public Worker{
    
    Texts &texts;
    std::vector<Segments> &temp;
    const std::vector<std::size_t> &spans;
    const SetNgrams &set_patterns;
    const bool &remove;
    const int &position;
    
    // Constructor
    segment_mt(Texts &texts_, std::vector<Segments> &temp_,
            const std::vector<std::size_t> &spans_, const SetNgrams &set_patterns_, const bool &remove_, 
            const int &position_):
            texts(texts_), temp(temp_), spans(spans_), set_patterns(set_patterns_), remove(remove_), position(position_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            temp[h] = segment(texts[h], spans, set_patterns, remove, position);
        }
    }
};


/* 
 * This function split tokens into segments by given patterns
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_segment()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param patterns_ list of target patterns
 * @param remove remove and save matched patterns if true
 * @param position determine position to split texts 1=before; 2=after
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_segment(const List &texts_,
                             const CharacterVector types_,
                             const List &patterns_,
                             const bool &remove,
                             const int &position){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");
    
    SetNgrams set_patterns;
    std::vector<std::size_t> spans = register_ngrams(patterns_, set_patterns);
    
    // dev::Timer timer;
    std::vector<Segments> temp(texts.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
     segment_mt segment_mt(texts, temp, spans, set_patterns, remove, position);
     parallelFor(0, texts.size(), segment_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = segment(texts[h], spans, set_patterns, remove, position);
    }
#endif
    
    // Get total number of matches
    std::size_t len = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        len += temp[h].size();
    }
    
    Texts segments(len);
    IntegerVector ids_document_(len);
    //CharacterVector names_document_(len), matches_(len);
    CharacterVector matches_(len);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Segments targets = temp[h];
        if (targets.size() == 0) continue;
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
                matches_[j] = join_strings(match, types_, " ");
            } else {
                matches_[j] = "";
            }
            
            ids_document_[j] = (int)h + 1;
            //names_document_[j] = names_[h];
            j++;
        }
    }
    
    Tokens segments_ = recompile(segments, types, remove, false, is_encoded(types_));
    segments_.attr("pattern") = matches_;
    //segments_.attr("docid") = names_document_;
    segments_.attr("docnum") = ids_document_;
    return(segments_);
}

/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
#qatd_cpp_tokens_segment(toks, letters, list(c(5, 6), 10), TRUE, 2)
qatd_cpp_tokens_segment(toks, letters, list(c(5, 6), 10), FALSE, 1)

#qatd_cpp_tokens_segment(toks, letters, list(c(5, 6), 10), FALSE, 2)

# 
# qatd_cpp_tokens_segment(toks, letters, list(10))
# qatd_cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# qatd_cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# qatd_cpp_tokens_segment(toks, letters, c(3, 4, 7))
# qatd_cpp_tokens_segment(toks, letters, c(3, 4, 7))


*/
