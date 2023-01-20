//#include "dev.h"
#include "lib.h"
#include "recompile.h"

using namespace quanteda;

// [[Rcpp::export]]
List qatd_cpp_tokens_nothing(const List &texts_,
                             bool old = true){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    List result_;
    if (old) {
      result_ = Rcpp::wrap(texts);
    } else {
      result_ = as_list(texts);
    }
    return result_;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#toks <- list(rep(1:10, 1))
#from <- list(c(1, 2), c(2, 3), c(5, 6), 10, 20)
#to <- list(c(1, 1, 1), c(2, 2, 2), c(5, 5, 5), 1, 2)
from <- list(c(9, 10))
to <- list(0)

#qatd_cpp_tokens_replace(toks, letters, dict, integer(0), 0)
qatd_cpp_tokens_nothing(toks)
#qatd_cpp_tokens_replace(toks, letters, from, to)

*/
