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


Text replace(Text tokens, 
             int span_max,
             MapNgrams &map_words){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = std::numeric_limits<unsigned int>::max(); // use largest limit as filler
    bool match = false;
    for(int span = span_max; span >= 1; span--){ // substitution starts from the longest sequences
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            unsigned int &id = map_words[ngram];
            if(id){
                match = true;
                std::fill(tokens.begin() + i + 1, tokens.begin() + i + span, filler); // fill subsequent tokens
                tokens[i] = id;
            }
        }
    }
    if(match) tokens.erase(std::remove(tokens.begin(), tokens.end(), filler), tokens.end());
    return tokens;
}

struct replace_mt : public Worker{
    
    Texts &input;
    Texts &output;
    int span_max;
    MapNgrams &map_words;
    
    // Constructor
    replace_mt(Texts &input_, Texts &output_, int span_max_, MapNgrams &map_words_):
              input(input_), output(output_), span_max(span_max_), map_words(map_words_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            output[h] = replace(input[h], span_max, map_words);
        }
    }
};


// [[Rcpp::export]]
List qatd_cpp_tokens_replace(List texts_, 
                             List words,
                             IntegerVector ids){
    
    Texts input = Rcpp::as<Texts>(texts_);

    MapNgrams map_words;
    int span_max = 0;
    for(int g = 0; g < words.size(); g++){
        if(has_na(words[g])) continue;
        Ngram word = words[g];
        Rcout << "Add " << ids[g] << "\n";
        map_words[word] = ids[g];
        if(span_max < word.size()) span_max = word.size();
    }
    //Rcout << "Span max " << span_max << "\n";
    
    Texts output(input.size());
    replace_mt replace_mt(input, output, span_max, map_words);
    
    dev::Timer timer;
    dev::start_timer("Token replace", timer);
    parallelFor(0, input.size(), replace_mt);
    dev::stop_timer("Token replace", timer);
    
    ListOf<IntegerVector> texts_key = Rcpp::wrap(output);
    
    return texts_key;
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
id <- rep(1, length(dict))
qatd_cpp_tokens_replace(toks, dict, id)



*/
