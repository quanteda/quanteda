#include "tokens.h"
#include "dev.h"
//#include "recompile.h"
using namespace quanteda;

// [[Rcpp::export]]
TokensPtr qatd_cpp_as_xptr(const List text_, 
                              const CharacterVector types_) {
    
    Texts texts = Rcpp::as<Texts>(text_);
    Types types = Rcpp::as<Types>(types_);
    TokensObj *ptr = new TokensObj(texts, types);
    return TokensPtr(ptr, true);
}

// [[Rcpp::export]]
TokensPtr qatd_cpp_copy_xptr(TokensPtr xptr) {
    TokensObj *ptr_copy = new TokensObj(xptr->texts, xptr->types);
    return TokensPtr(ptr_copy, true);
}

// [[Rcpp::export]]
List qatd_cpp_as_list(TokensPtr xptr) {
    xptr->recompile();
    Tokens texts_ = as_list(xptr->texts);
    texts_.attr("types") = encode(xptr->types);;
    texts_.attr("padding") = xptr->padding;
    texts_.attr("class") = "tokens";
    return texts_;
}

// [[Rcpp::export]]
TokensPtr qatd_cpp_subset(TokensPtr xptr, IntegerVector index_) {
    Texts texts(index_.size());
    for (unsigned int i = 0; i < (unsigned int)index_.size(); i++) {
        if (index_[i] < 1 || index_[i] - 1 >= xptr->texts.size()) {
            throw std::range_error("Invalid document index");
        }
        texts[i] = xptr->texts[index_[i] - 1];
    }
    xptr->texts = texts;
    return xptr;
}

// [[Rcpp::export]]
int qatd_cpp_ndoc(TokensPtr xptr) {
    return xptr->texts.size();
}

// [[Rcpp::export]]
IntegerVector qatd_cpp_ntoken(TokensPtr xptr) {
    Rcout << "qatd_cpp_ntoken()\n";
    xptr->recompile();
    std::size_t H = xptr->texts.size();
    IntegerVector ls_(H);
    for (std::size_t h = 0; h < H; h++) {
        ls_[h] = xptr->texts[h].size();
    }
    return ls_;
}

// [[Rcpp::export]]
CharacterVector qatd_cpp_types(TokensPtr xptr) {
    Rcout << "qatd_cpp_types()\n";
    xptr->recompile();
    return encode(xptr->types);
}


/***R
toks <- rep(list(sample(100)), 100)
xtoks <- qatd_cpp_as_xptr(toks, letters)
toks <- qatd_cpp_nothing(xtoks)
qatd_cpp_as_list(xtoks)

*/
