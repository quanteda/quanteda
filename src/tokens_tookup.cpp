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
            Text tokens_loc,
            int id,
            int span_max,
            SetNgrams &keys){
    
    for(int span = 1; span <= span_max; span++){
        //Rcout << "Span " << span << "\n";
        for(int i = 0; i < tokens.size() - (span - 1); i++){
            Ngram tokens_sub(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = keys.find(tokens_sub) != keys.end();
            if(is_in){
                tokens_loc[i] = id;
            }
        }
    }
    return tokens_loc;
}


struct lookup_mt : public Worker{
    
    Texts &input;
    Texts &output;
    int id;
    int span_max;
    SetNgrams &set_keys;
    
    // Constructor
    lookup_mt(Texts &input_, Texts &output_, int id_, int span_max_, SetNgrams &set_keys_):
              input(input_), output(output_), id(id_), span_max(span_max_), set_keys(set_keys_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            output[h] = lookup(input[h], output[h], id, span_max, set_keys);
        }
    }
};


// [[Rcpp::export]]
List qatd_cpp_lookup_int_list(List texts_, 
                              List texts_loc_,
                              List keys,
                              int id){
    
    SetNgrams set_keys;
    int span_max = 0;
    for(int g = 0; g < keys.size(); g++){
        if(has_na(keys[g])) continue;
        Ngram key = keys[g];
        set_keys.insert(key);
        if(span_max < key.size()) span_max = key.size();
    }
    //Rcout << "Span max " << span_max << "\n";
    
    Texts input = Rcpp::as< Texts >(texts_);
    Texts output = Rcpp::as< Texts >(texts_loc_);
    
    lookup_mt lookup_mt(input, output, id, span_max, set_keys);
    parallelFor(0, input.size(), lookup_mt);
    
    ListOf<IntegerVector> texts_keys = Rcpp::wrap(output);
    texts_keys.attr("class") = "tokens";
    
    return(texts_keys);
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
toks_loc <- qatd_cpp_structcopy_int_list(toks)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- mapply(c, 1:1000, 1:1000*2, SIMPLIFY = FALSE)
#dict <- list(c(1, NA))
#dict <- as.list(c(1:1000))
qatd_cpp_lookup_int_list(toks, toks_loc, dict, 99)



*/
