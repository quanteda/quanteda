#include "lib.h"
#include "dev.h"
#include "recompile.h"
using namespace quanteda;

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
    //List texts_ = Rcpp::wrap(xptr->texts);
    //return texts_;
    return recompile(xptr->texts, xptr->types, true, true, true);
}

// [[Rcpp::export]]
int qatd_cpp_ndoc(TokensPtr xptr) {
    return xptr->texts.size();
}

// [[Rcpp::export]]
CharacterVector qatd_cpp_types(TokensPtr xptr) {
    return encode(xptr->types);
}

/***R
toks <- rep(list(sample(100)), 100)
xtoks <- qatd_cpp_as_xptr(toks, letters)
toks <- qatd_cpp_nothing(xtoks)
qatd_cpp_as_list(xtoks)

*/
