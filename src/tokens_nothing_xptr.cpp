#include "lib.h"
#include "dev.h"
#include "recompile.h"
using namespace quanteda;

/* 
 * This function selects features in tokens object with multiple threads. 
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
Xtokens qatd_cpp_nothing(Xtokens &xptr,
                            const CharacterVector types_
){
    
    Texts texts = *xptr;
    //Texts texts = &xptr;
    
    for (std::size_t h = 0; h < texts.size(); h++) {
        dev::print_ngram(texts[h]);
    }
    
    // dev::stop_timer("Token select", timer);
    //return recompile(texts, types, true, false, is_encoded(types_));
    return Xtokens(&texts);
}

// [[Rcpp::export]]
Xtokens qatd_cpp_as_xtokens(const List &texts_) {
    Texts texts = Rcpp::as<Texts>(texts_);
    return Xtokens(&texts);
}

// [[Rcpp::export]]
List qatd_cpp_as_tokens(Xtokens &xptr) {
    Texts texts = *xptr;
    Tokens texts_ = Rcpp::wrap(texts);
    return texts_;
}

/***R
toks <- list(rep(1:10, 1))
xtoks <- qatd_cpp_as_xtokens(toks)
qatd_cpp_nothing(xtoks, letters)
qatd_cpp_as_tokens(xtoks)

*/
