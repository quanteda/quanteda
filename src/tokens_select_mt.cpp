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


Text keep(Text tokens, 
          int span_max,
          SetNgrams &set_words,
          bool padding){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Text tokens_copy(tokens.size(), 0);
    for(int span = span_max; span >= 1; span--){ // substitution starts from the longest sequences
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_words.find(ngram) != set_words.end();
            if(is_in){
                std::copy(ngram.begin(), ngram.end(), tokens_copy.begin() + i);
            }
        }
    }
    if(!padding) tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), 0), tokens_copy.end());
    return tokens_copy;
}

Text remove(Text tokens, 
            int span_max,
            SetNgrams &set_words,
            bool padding){

    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    Text tokens_copy(tokens.size(), 0);
    unsigned int filler = std::numeric_limits<unsigned int>::max(); // use upper limit as a filler
    bool match = false;
    for(int span = span_max; span >= 1; span--){ // substitution starts from the longest sequences
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_words.find(ngram) != set_words.end();
            if(is_in){
                match = true;
                std::fill(tokens.begin() + i, tokens.begin() + i + span, filler); // fill subsequent containers
                if(padding) tokens[i] = 0;
            }
        }
    }
    if(match) tokens.erase(std::remove(tokens.begin(), tokens.end(), filler), tokens.end());
    return tokens;
}

struct select_mt : public Worker{
    
    Texts &input;
    Texts &output;
    int span_max;
    SetNgrams &set_words;
    bool padding;
    int mode;
    
    // Constructor
    select_mt(Texts &input_, Texts &output_, int span_max_, SetNgrams &set_words_, int mode_, bool padding_):
              input(input_), output(output_), span_max(span_max_), set_words(set_words_), mode(mode_), padding(padding_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        if(mode == 1){
            for (int h = begin; h < end; h++){
                output[h] = keep(input[h], span_max, set_words, padding);
            }
        }else if(mode == 2){
            for (int h = begin; h < end; h++){
                output[h] = remove(input[h], span_max, set_words, padding);
            }
        }else{
            for (int h = begin; h < end; h++){
                output[h] = input[h];
            }
        }
    }
};

/* 
 * This funciton select features in tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_select(List texts_, 
                            List words_,
                            int mode_,
                            bool padding_){
    
    Texts input = Rcpp::as<Texts>(texts_);
    List words = words_;
    int mode = mode_;
    bool padding = padding_;



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
    select_mt select_mt(input, output, span_max, set_words, mode, padding);
    
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
qatd_cpp_tokens_select(toks, dict, TRUE, 1)
qatd_cpp_tokens_select(toks, dict, FALSE, 1)



*/
