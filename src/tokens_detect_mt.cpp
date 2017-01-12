#include <Rcpp.h>
//#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;


Text detect(Text tokens, 
            size_t span_max,
            SetNgrams set_words){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Text tokens_pos(tokens.size(), 0);
    for (std::size_t span = span_max; span > 0; span--){ // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_words.find(ngram) != set_words.end();
            if(is_in){
                std::fill(tokens_pos.begin() + i, tokens_pos.begin() + i + span, 1);
            }
        }
    }
    return tokens_pos;
}


struct detect_mt : public Worker{
    
    Texts &input;
    Texts &output;
    size_t span_max;
    SetNgrams &set_words;
    
    // Constructor
    detect_mt(Texts &input_, Texts &output_, size_t span_max_, SetNgrams &set_words_):
              input(input_), output(output_), span_max(span_max_), set_words(set_words_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            output[h] = detect(input[h], span_max, set_words);
        }
    }
};

/* 
 * This funciton finds features in tokens object. This is similar to tokens_tookup, 
 * but returns a tokens object filled with zero or one.
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used kwic()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to find
 * 
 */


// [[Rcpp::export]]
List qatd_cpp_tokens_detect(List texts_, 
                            List words_){
    
    Texts input = Rcpp::as<Texts>(texts_);
    List words = words_;

    SetNgrams set_words;
    std::size_t span_max = 0;
    for (unsigned int g = 0; g < words.size(); g++) {
        if (has_na(words[g])) continue;
        Ngram word = words[g];
        set_words.insert(word);
        if (span_max < word.size()) span_max = word.size();
    }
    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Dictionary detect", timer);
    #if RCPP_PARALLEL_USE_TBB
    detect_mt detect_mt(input, output, span_max, set_words);
    parallelFor(0, input.size(), detect_mt);
    #else
    for (std::size_t h = 0; h < input.size(); h++){
        output[h] = detect(input[h], span_max, set_words);
    }
    #endif
    // dev::stop_timer("Dictionary detect", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    return texts_list;
}

/***R

toks <- rep(list(rep(1:10, 10), rep(5:15, 10)), 1000)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)

microbenchmark::microbenchmark(
out=qatd_cpp_tokens_detect(toks, dict),
times=1000
)
*/
