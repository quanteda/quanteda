#include "quanteda.h"
#include "recompile.h"
//#include "dev.h"
using namespace quanteda;

typedef pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

Targets segment(Text tokens,
                const std::vector<std::size_t> &spans,
                const SetNgrams &set_patterns,
                const bool & remove){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Targets targets;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_patterns.find(ngram) != set_patterns.end();
            if (is_in) {
                targets.push_back(make_pair(i, i + span - 1));
            }
        }
    }
    
    Targets segments;
    if (targets.size() == 0) {
        segments.push_back(make_pair(0, tokens.size()));
        return segments;
    }
    
    // sort by the starting positions
    std::sort(targets.begin(), targets.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
        return left.first < right.first;
    });
    
    for (size_t i = 0; i < targets.size(); i++) {
        int from = 0;
        if (i > 0) {
            from = targets[i - 1].second + 1;
        }
        int to;
        if (remove) {
            to = targets[i].first - 1;
        } else {
            to = targets[i].second;
        }
        segments.push_back(make_pair(from, to));
        // Rcout << "segment " << from << " " << to << "\n";
    }
    
    // add segment till end
    size_t last = targets.size() - 1;
    // Rcout << "last " << last << "\n";
    // Rcout << "targets[last] " << targets[last].second << "\n";
    if (targets[last].second < tokens.size() - 1) {
        segments.push_back(make_pair(targets[last].second + 1, tokens.size() - 1));
    }
    
    return segments;
}

struct segment_mt : public Worker{
    
    Texts &texts;
    std::vector<Targets> &temp;
    const std::vector<std::size_t> &spans;
    const SetNgrams &set_patterns;
    const bool &remove;
    
    // Constructor
    segment_mt(Texts &texts_, std::vector<Targets> &temp_,
            const std::vector<std::size_t> &spans_, const SetNgrams &set_patterns_, const bool & remove_):
            texts(texts_), temp(temp_), spans(spans_), set_patterns(set_patterns_), remove(remove_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            temp[h] = segment(texts[h], spans, set_patterns, remove);
        }
    }
};


/* 
 * This funciton split tokens into segments by given patterns
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_segment()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param patterns_ list of target patterns
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_segment(const List &texts_,
                             const CharacterVector types_,
                             const List &patterns_,
                             const bool &remove){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");
    
    SetNgrams set_patterns;
    std::vector<std::size_t> spans = register_ngrams(patterns_, set_patterns);
    
    // dev::Timer timer;
    std::vector<Targets> temp(texts.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
    segment_mt segment_mt(texts, temp, spans, set_patterns, remove);
    parallelFor(0, texts.size(), segment_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = segment(texts[h], spans, set_patterns, remove);
    }
#endif
    
    // Get total number of matches
    std::size_t len = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        len += temp[h].size();
    }
    
    Texts segments(len);
    IntegerVector document_ids_(len), segment_ids_(len);
    CharacterVector document_names_(len);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Targets targets = temp[h];
        if (targets.size() == 0) continue;
        Text tokens = texts[h];
        for (size_t i = 0; i < targets.size(); i++) {
            Target target = targets[i];
            // Rcout << "target " << target.first << " " << target.second << "\n";
            Text segment(tokens.begin() + target.first, tokens.begin() + target.second + 1);
            segments[j] = segment;
            document_ids_[j] = (int)h + 1;
            segment_ids_[j] = (int)i + 1;
            document_names_[j] = names_[h];
            j++;
        }
    }
    
    Tokens segments_ = recompile(segments, types, false, false, false);
    segments_.attr("document") = document_names_;
    segments_.attr("docid") = document_ids_;
    segments_.attr("segid") = segment_ids_;
    
    return(segments_);
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_tokens_segment(toks, letters, list(c(5, 6), 10), TRUE)
qatd_cpp_tokens_segment(toks, letters, list(c(5, 6), 10), FALSE)

# 
# qatd_cpp_tokens_segment(toks, letters, list(10))
# qatd_cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# qatd_cpp_tokens_segment(toks, letters, list(c(3, 4), 7))
# qatd_cpp_tokens_segment(toks, letters, c(3, 4, 7))
# qatd_cpp_tokens_segment(toks, letters, c(3, 4, 7))


*/
