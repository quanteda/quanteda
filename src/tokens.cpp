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
ListOf<CharacterVector> qatd_cpp_unhash(ListOf<IntegerVector> list_int, 
                                        CharacterVector types){
    
    types.push_front(""); // offset types to match index in R
    List list_chr(list_int.size());
    for (unsigned int i = 0; i < list_int.size(); i++){
      list_chr[i] = types[list_int[i]];
    }
    return list_chr;
}


/*** R

tok <- list(c(5,6,8),c(11,25))
stringi::stri_c_list(qatd_cpp_unhash(tok, letters))


*/
