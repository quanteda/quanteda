// [[Rcpp::depends(stringi)]]
#include <RcppParallel.h>
#include <unordered_map>
#include <unordered_set>
//#include "dev.h"

// [[Rcpp::depends(stringi)]]
#include <Rcpp.h>
using namespace Rcpp;

#include <stringi.cpp>

typedef std::vector<std::string> Corpus;
typedef std::vector<std::string> Text;
typedef std::vector<Text> Texts;

// [[Rcpp::export]]
SEXP cpp_tokenize(CharacterVector corpus_){
    
    Corpus corpus = Rcpp::as<Corpus>(corpus_);
    std::size_t H = corpus.size();
    Texts texts(H);

//#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        IntegerVector index = seq(r.begin(), r.end() - 1);
        CharacterVector corpus_ = Rcpp::wrap(corpus);
        List temp_ = stri_split_boundaries(corpus_[index]);
        Texts temp = Rcpp::as<Texts>(temp_);
        for (int h = r.begin(); h < r.end(); ++h) {
            texts[h] = temp[h];
        }
    });
// #else
//     IntegerVector index = seq(0, H - 1);
//     List temp_ = stri_split_boundaries(corpus_[index]);
//     Texts temp = Rcpp::as<Texts>(temp_);
//     for (int h = 0; h < H; ++h) {
//         texts[h] = temp[h];
//     }
// #endif
    
    return Rcpp::wrap(texts);
//    TokensObj *ptr = new TokensObj(temp, types, true, true);
//    return TokensPtr(ptr, true);
}

/***R

out <- cpp_tokenize(letters)

*/
