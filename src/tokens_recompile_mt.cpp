//#include "dev.h"
#include "quanteda.h"
#include "recompile.h"
using namespace quanteda;

/* 
* This funciton recompiles tokens object.
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types in tokens
*/

// [[Rcpp::export]]
List qatd_cpp_tokens_recompile(const List &texts_, 
                               const CharacterVector types_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    return recompile(texts, types);
    
}

/***R

#toks3 <- list(rep(0:5, 1), rep(10:15, 1))
toks3 <- list(0:26)
qatd_cpp_tokens_recompile(toks3, letters)
#qatd_cpp_tokens_recompile(toks3, rep(c('a', 'b', 'c'), 7))



*/
