#include "lib.h"
#include "dev.h"
#include "recompile.h"
using namespace quanteda;

/* 
 * This function selects features in tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param text_ tokens ojbect
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * 
 */

// [[Rcpp::export]]
TokensPtr qatd_cpp_nothing(TokensPtr xptr){
    Texts texts = xptr->texts;
    for (std::size_t h = 0; h < xptr->texts.size(); h++) {
        texts[h].push_back(0);
        xptr->texts[h] =  texts[h];
    }
    return xptr;
    //return TokensPtr(xptr, true);
}

// [[Rcpp::export]]
TokensPtr qatd_cpp_as_xptr(const List text_, 
                              const CharacterVector types_) {
    
    Texts texts = Rcpp::as<Texts>(text_);
    Types types = Rcpp::as<Types>(types_);
    TokensObj *ptr = new TokensObj(texts, types);
    return TokensPtr(ptr, true);
}

// [[Rcpp::export]]
List qatd_cpp_as_list(TokensPtr xptr) {
    //Texts texts = xptr->texts;
    List texts_ = Rcpp::wrap(xptr->texts);
    return texts_;
}

/***R
toks <- rep(list(sample(100)), 100)
xtoks <- qatd_cpp_as_xptr(toks, letters)
toks <- qatd_cpp_nothing(xtoks)
qatd_cpp_as_list(xtoks)

*/
