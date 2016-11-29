#include <Rcpp.h>
using namespace Rcpp;

/*
 * This function is reconstrct character tokens from integer tokens and types.
 * Equivalent to lapply(tokens, function(x, y) y[x], types)
 * @creator Kohei Watanabe
 * @used ngrams.R
 */

// [[Rcpp::export]]
List qatd_cpp_unhash(ListOf<IntegerVector> tokens_int, CharacterVector types){
    types.push_front(""); // offset types to match index in C++
    ListOf<CharacterVector> tokens_chr(tokens_int.size());
    for(int i=0; i < tokens_int.size(); i++){
        tokens_chr[i] = types[tokens_int[i]];
    }
    return tokens_chr;
}