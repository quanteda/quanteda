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


Text select(Text tokens, 
            int span_max,
            SetNgrams &set_words,
            bool padding){
    
    Text tokens_copy(tokens.size(), 0);
    bool match = false;
    for(int span = span_max; span >= 1; span--){ // substitution starts from the longest sequences
        //for(int span = 1; span <= span_max; span++){ // substitution starts from the shortest sequences
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_words.find(ngram) != set_words.end();
            if(is_in){
                match = true;
                std::copy(ngram.begin(), ngram.end(), tokens_copy.begin() + i);
            }
        }
    }
    if(!padding) tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), 0), tokens_copy.end());
    return tokens_copy;
}

struct select_mt : public Worker{
    
    Texts &input;
    Texts &output;
    int span_max;
    SetNgrams &set_words;
    bool padding;
    
    
    // Constructor
    select_mt(Texts &input_, Texts &output_, int span_max_, SetNgrams &set_words_, bool padding_):
              input(input_), output(output_), span_max(span_max_), set_words(set_words_), padding(padding_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            output[h] = select(input[h], span_max, set_words, padding);
        }
    }
};


// [[Rcpp::export]]
List qatd_cpp_tokens_select(List texts_, 
                             List words,
                             bool padding){
    
    Texts input = Rcpp::as<Texts>(texts_);

    SetNgrams set_words;
    int span_max = 0;
    for(int g = 0; g < words.size(); g++){
        if(has_na(words[g])) continue;
        Ngram word = words[g];
        set_words.insert(word);
        if(span_max < word.size()) span_max = word.size();
    }
    //Rcout << "Span max " << span_max << "\n";
    
    Texts output(input.size());
    select_mt select_mt(input, output, span_max, set_words, padding);
    
    dev::Timer timer;
    dev::start_timer("Token select", timer);
    parallelFor(0, input.size(), select_mt);
    dev::stop_timer("Token select", timer);
    
    ListOf<IntegerVector> texts_key = Rcpp::wrap(output);
    
    return texts_key;
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
qatd_cpp_tokens_select(toks, dict, TRUE)
qatd_cpp_tokens_select(toks, dict, FALSE)



*/
