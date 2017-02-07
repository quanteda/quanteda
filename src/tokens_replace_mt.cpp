#include <Rcpp.h>
//#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;


Text replace(Text tokens, 
             const std::vector<std::size_t> &spans,
             const MapNgrams &map_words){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = UINT_MAX; // use largest limit as filler
    bool match = false;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_words.find(ngram);
            if (it != map_words.end()) {
                //unsigned int id = map_words[ngram];
                //if (id) {
                match = true;
                std::fill(tokens.begin() + i + 1, tokens.begin() + i + span, filler); // fill subsequent tokens
                tokens[i] = it->second;
            }
        }
    }
    if (match) tokens.erase(std::remove(tokens.begin(), tokens.end(), filler), tokens.end());
    return tokens;
}

struct replace_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<std::size_t> &spans;
    const MapNgrams &map_words;
    
    // Constructor
    replace_mt(Texts &input_, Texts &output_, const std::vector<std::size_t> &spans_, const MapNgrams &map_words_):
              input(input_), output(output_), spans(spans_), map_words(map_words_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            output[h] = replace(input[h], spans, map_words);
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
* 
*/

// [[Rcpp::export]]
List qatd_cpp_tokens_replace(const List &texts_, 
                             const List &words_,
                             const IntegerVector &ids_){
    
    Texts input = Rcpp::as<Texts>(texts_);
    const List words = words_;
    const IntegerVector ids = ids_;
    
    MapNgrams map_words;
    std::vector<std::size_t> spans(words.size());
    for (unsigned int g = 0; g < words.size(); g++) {
        if (has_na(words[g])) continue;
        Ngram word = words[g];
        map_words[word] = ids[g];
        spans[g] = word.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Token replace", timer);
#if QUANTEDA_USE_TBB
    replace_mt replace_mt(input, output, spans, map_words);
    parallelFor(0, input.size(), replace_mt);
#else
    for (std::size_t h = 0; h < input.size(); h++) {
        output[h] = replace(input[h], spans, map_words);
    }
#endif
    // dev::stop_timer("Token replace", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    return texts_list;
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
id <- rep(1, length(dict))
qatd_cpp_tokens_replace(toks, dict, id)



*/
