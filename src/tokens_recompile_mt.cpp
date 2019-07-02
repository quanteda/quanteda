//#include "dev.h"
#include "lib.h"
#include "recompile.h"
using namespace quanteda;

/* 
* This function recompiles tokens object.
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types in tokens
* @param gap if TRUE, remove gaps between token IDs
* @param dup if TRUE, merge duplicated token types into the same ID 
*/

// [[Rcpp::export]]
List qatd_cpp_tokens_recompile(const List &texts_, 
                               const CharacterVector types_,
                               const bool gap = true,
                               const bool dup = true){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    return recompile(texts, types, gap, dup);
    
}

/***R

#toks3 <- list(rep(0:5, 1), rep(10:15, 1))
toks3 <- list(0:26)
qatd_cpp_tokens_recompile(toks3, letters)

toks4 <- list(c(1:5))
qatd_cpp_tokens_recompile(toks4, c('あ', 'い', 'う', 'え', 'お'))



*/
