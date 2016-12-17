#include <Rcpp.h>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;


Text lookup(Text tokens, 
            int span_max,
            MapNgrams &map_keys){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Text keys;
    keys.reserve(tokens.size());
    for(int span = 1; span <= span_max; span++){
        //Rcout << "Span " << span << "\n";
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            unsigned int &id_keys = map_keys[ngram];
            if(id_keys){
                //Rcout << "Found " << id_keys << "\n";
                keys.push_back(id_keys);
            }
        }
    }
    return keys;
}


struct lookup_mt : public Worker{
    
    Texts &input;
    Texts &output;
    int span_max;
    MapNgrams &map_keys;
    
    // Constructor
    lookup_mt(Texts &input_, Texts &output_, int span_max_, MapNgrams &map_keys_):
              input(input_), output(output_), span_max(span_max_), map_keys(map_keys_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            output[h] = lookup(input[h], span_max, map_keys);
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
List qatd_cpp_tokens_lookup(List texts_, 
                            List words_,
                            IntegerVector ids_){
    
    Texts input = Rcpp::as<Texts>(texts_);
    List words = words_;
    IntegerVector ids = ids_;

    MapNgrams map_words;
    int span_max = 0;
    for(int g = 0; g < words.size(); g++){
        if(has_na(words[g])) continue;
        Ngram word = words[g];
        map_words[word] = ids_[g];
        if(span_max < word.size()) span_max = word.size();
    }
    //Rcout << "Span max " << span_max << "\n";
    
    Texts output(input.size());
    lookup_mt lookup_mt(input, output, span_max, map_words);
    
    dev::Timer timer;
    dev::start_timer("Dictionary lookup", timer);
    parallelFor(0, input.size(), lookup_mt);
    dev::stop_timer("Dictionary lookup", timer);
    
    ListOf<IntegerVector> texts_key = Rcpp::wrap(output);
    
    return texts_key;
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
key <- 1:length(dict)
qatd_cpp_tokens_lookup(toks, dict, key)



*/
