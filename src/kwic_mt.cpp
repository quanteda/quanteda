#include "lib.h"
#include "recompile.h"
#include "dev.h"
using namespace quanteda;

typedef std::tuple<unsigned int, size_t, size_t> Target;
typedef std::vector<Target> Targets;

Targets kwic(Text tokens,
                   const std::vector<std::size_t> &spans,
                   const MultiMapNgrams &map_pats,
                   UintParam &match_count){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Targets targets;
    targets.reserve(tokens.size());
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_pats.equal_range(ngram);
            for (auto it = range.first; it != range.second; ++it) {
                unsigned int pat = it->second;
                targets.push_back(std::make_tuple(pat, i, i + span - 1));
                match_count++;
            }
        }
    }
    
    // sort by the starting positions
    //std::sort(targets.begin(), targets.end(), [](const std::pair<int,int> &left, const std::pair<int,int> &right) {
    //   return left.first < right.first;
    //});
    return targets;
}

struct kwic_mt : public Worker{
    
    Texts &texts;
    std::vector<Targets> &temp;
    const std::vector<std::size_t> &spans;
    const MultiMapNgrams &map_pats;
    UintParam &match_count;
    
    // Constructor
    kwic_mt(Texts &texts_, std::vector<Targets> &temp_,
            const std::vector<std::size_t> &spans_, const MultiMapNgrams &map_pats_,
            UintParam &match_count_):
            texts(texts_), temp(temp_), spans(spans_), map_pats(map_pats_), 
            match_count(match_count_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            temp[h] = kwic(texts[h], spans, map_pats, match_count);
        }
    }
};


/* 
 * This function generate generates keyword-in-contexts. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used kwic()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param words_ list of target features
 * @param pats_ index for patterns
 * @param delim_ character to join tokens
 */

// [[Rcpp::export]]
DataFrame qatd_cpp_kwic(const List &texts_,
                        const CharacterVector types_,
                        const List &words_,
                        const IntegerVector &pats_,
                        const unsigned int &window,
                        const String &delim_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");

    MultiMapNgrams map_pats;
    map_pats.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams words = Rcpp::as<Ngrams>(words_);
    std::vector<unsigned int> pats = Rcpp::as< std::vector<unsigned int> >(pats_);
    
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
    std::vector<Targets> temp(texts.size());
    
    //dev::start_timer("Search keywords", timer);
    UintParam match_count = 0;
#if QUANTEDA_USE_TBB
    kwic_mt kwic_mt(texts, temp, spans, map_pats, match_count);
    parallelFor(0, texts.size(), kwic_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = kwic(texts[h], spans, map_pats, match_count);
    }
#endif
    //dev::stop_timer("Search keywords", timer);
    
    //dev::start_timer("Create strings", timer);
    IntegerVector documents_(match_count), segments_(match_count);
    IntegerVector pat_(match_count), pos_from_(match_count), pos_to_(match_count);
    CharacterVector coxs_name_(match_count), coxs_pre_(match_count);
    CharacterVector coxs_target_(match_count), coxs_post_(match_count);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Targets targets = temp[h];
        if (targets.size() == 0) continue;
        Text tokens = texts[h];
        int last = (int)tokens.size() - 1;
        for (size_t i = 0; i < targets.size(); i++) {
            Target target = targets[i];
            int from = std::get<1>(target) - window;
            int to = std::get<2>(target) + window;
            //Rcout << j << " " << std::get<1>(target) << ":" << std::get<2>(target) << "\n";
            
            // Save as intergers
            documents_[j] = (int)h + 1;
            segments_[j] = (int)i + 1;
            
            // Save as strings
            Text cox_pre(tokens.begin() + std::max(0, from), tokens.begin() + std::get<1>(target));
            Text cox_target(tokens.begin() + std::get<1>(target), tokens.begin() + std::get<2>(target) + 1);
            Text cox_post(tokens.begin() + std::get<2>(target) + 1, tokens.begin() + std::min(to, last) + 1);
            
            pat_[j] = std::get<0>(target);
            pos_from_[j] = std::get<1>(target) + 1;
            pos_to_[j] = std::get<2>(target) + 1;

            coxs_pre_[j] = join_strings(cox_pre, types_, delim_); 
            coxs_target_[j] = join_strings(cox_target, types_, delim_);
            coxs_post_[j] = join_strings(cox_post, types_, delim_);
            coxs_name_[j] = names_[h];
            j++;
        }
    }
    //dev::stop_timer("Create strings", timer);
    //dev::start_timer("Create data.frame", timer);
    DataFrame output_ = DataFrame::create(_["docname"] = coxs_name_,
                                          _["from"]    = pos_from_,
                                          _["to"]      = pos_to_,
                                          _["pre"]     = coxs_pre_,
                                          _["keyword"] = coxs_target_,
                                          _["post"]    = coxs_post_,
                                          _["pattern"] = pat_,
                                          _["stringsAsFactors"] = false);
    //dev::stop_timer("Create data.frame", timer);
    output_.attr("docid") = documents_;
    output_.attr("segid") = segments_;
    return output_;
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_kwic(toks, letters, list(10), 1, "_")
qatd_cpp_kwic(toks, letters, list(10), 1, "_")
qatd_cpp_kwic(toks, letters, list(c(3, 4), 7), 2, "_")
qatd_cpp_kwic(toks, letters, list(c(3, 4), 7), 2, "_")
qatd_cpp_kwic(toks, letters, c(3, 4, 7), 2, "_")
qatd_cpp_kwic(toks, letters, c(3, 4, 7), 2, "_")


*/
