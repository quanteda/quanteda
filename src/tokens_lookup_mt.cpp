#include <Rcpp.h>
//#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;


Text lookup(Text tokens, 
            const std::vector<std::size_t> &spans,
            const MultiMapNgrams &map_keys){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Text keys;
    keys.reserve(tokens.size());
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_keys.equal_range(ngram);
            for (auto it = range.first; it != range.second; ++it){
                //Rcout << it->second << "\n";
                keys.push_back(it->second);
            }
            
        }
    }
    std::sort(keys.begin(), keys.end()); // solve system dependency
    return keys;
}


struct lookup_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<std::size_t> &spans;
    const MultiMapNgrams &map_keys;
    
    // Constructor
    lookup_mt(Texts &input_, Texts &output_, std::vector<std::size_t> &spans_, MultiMapNgrams &map_keys_):
              input(input_), output(output_), spans(spans_), map_keys(map_keys_){}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            output[h] = lookup(input[h], spans, map_keys);
        }
    }
};

/* 
 * This funciton finds features in tokens object. This is similar to tokens_replace, 
 * but all overlapping or nested features are detected and recorded by IDs.
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_lookup()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to find
 * @param ids_ IDs of features
 * 
 */


// [[Rcpp::export]]
List qatd_cpp_tokens_lookup(const List &texts_, 
                            const List &keys_,
                            const IntegerVector &ids_){
    
    Texts input = Rcpp::as<Texts>(texts_);
    const List keys = keys_;
    const IntegerVector ids = ids_;

    MultiMapNgrams map_keys;
    std::vector<std::size_t> spans(keys.size());
    for (unsigned int g = 0; g < keys.size(); g++) {
        if (has_na(keys[g])) continue;
        Ngram word = keys[g];
        map_keys.insert(std::make_pair(word, ids[g]));
        spans[g] = word.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Dictionary lookup", timer);
    #if RCPP_PARALLEL_USE_TBB
    lookup_mt lookup_mt(input, output, spans, map_keys);
    parallelFor(0, input.size(), lookup_mt);
    #else
    for (std::size_t h = 0; h < input.size(); h++){
        output[h] = lookup(input[h], spans, map_keys);
    }
    #endif
    // dev::stop_timer("Dictionary lookup", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    return texts_list;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- list(1, 10, 20)
key <- 1:length(dict)
qatd_cpp_tokens_lookup(toks, dict, key)



*/
