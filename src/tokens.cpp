#include <Rcpp.h>
#include <string>
#include "quanteda.h"
using namespace Rcpp;
using namespace quanteda;


namespace tokens {
typedef std::vector< std::string > Types;
typedef std::vector< std::vector<unsigned int> > Hashed;
typedef std::vector< std::vector<std::string> > Unhashed;
}
using namespace tokens;

/*
 * This function is reconstrct character tokens from integer tokens and types.
 * Equivalent to lapply(tokens, function(x, y) y[x], types)
 * @creator Kohei Watanabe
 * @used ngrams.R
 */

// [[Rcpp::export]]
ListOf<CharacterVector> qatd_cpp_unhash(ListOf<IntegerVector> tokens_int, 
                                        CharacterVector types){
    
    types.push_front(""); // offset types to match index in R
    List tokens_chr(tokens_int.size());
    for (size_t i = 0; i < tokens_int.size(); i++){
        tokens_chr[i] = types[tokens_int[i]];
    }
    return tokens_chr;
}


/*** R

tok <- list(c(5,6,8),c(11,25))
stringi::stri_c_list(qatd_cpp_unhash(tok, letters))


*/
