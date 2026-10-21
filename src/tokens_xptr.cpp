#include "lib.h"
#include "dev.h"
//#include "recompile.h"
using namespace quanteda;


// [[Rcpp::export]]
TokensPtr cpp_xptr() {
    TokensObj *ptr = new TokensObj();
    return TokensPtr(ptr, true);
}

// [[Rcpp::export]]
TokensPtr cpp_as_xptr(const List text_,
                      const CharacterVector types_) {

    Texts texts = Rcpp::as<Texts>(text_);
    Types types = Rcpp::as<Types>(types_);
    TokensObj *ptr = new TokensObj(texts, types, true);
    return TokensPtr(ptr, true);
}

// [[Rcpp::export]]
TokensPtr cpp_copy_xptr(TokensPtr xptr) {
    TokensObj *ptr_copy = new TokensObj(xptr->texts,
                                        xptr->types,
                                        xptr->recompiled,
                                        xptr->padded);
    return TokensPtr(ptr_copy, true);
}

// [[Rcpp::export]]
List cpp_get_attributes(TokensPtr xptr) {
    List list_ = List::create(_["recompiled"] = xptr->recompiled,
                              _["padded"] = xptr->padded);
    return list_;
}

// [[Rcpp::export]]
List cpp_as_tokens(TokensPtr xptr) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_as_tokens()\n";
#endif
    xptr->recompile();
    List texts_ = as_list(xptr->texts);
    texts_.attr("types") = encode(xptr->types);
    texts_.attr("class") = "tokens";
    return texts_;
}

// [[Rcpp::export]]
List cpp_as_list(TokensPtr xptr) {
    List texts_ = wrap(as_list(xptr->texts));
    texts_.attr("types") = encode(xptr->types);
    return texts_;
}

// [[Rcpp::export]]
TokensPtr cpp_subset(TokensPtr xptr, IntegerVector index_) {
    std::vector<int> index = Rcpp::as< std::vector<int> >(index_);
    Texts texts(index.size());
    for (std::size_t i = 0; i < index.size(); i++) {
        if (index[i] < 1 || index[i] - 1 >= (int)xptr->texts.size()) {
            throw std::range_error("Invalid document index");
        }
        texts[i] = xptr->texts[index[i] - 1];
    }
    TokensObj *ptr_new = new TokensObj(texts, xptr->types, xptr->recompiled);
    return TokensPtr(ptr_new, true);
}

// [[Rcpp::export]]
int cpp_ndoc(TokensPtr xptr) {
    return xptr->texts.size();
}

// [[Rcpp::export]]
IntegerVector cpp_ntoken(TokensPtr xptr, bool no_padding = false) {

#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_ntoken()\n";
#endif
    return Rcpp::wrap(xptr->get_ntoken(no_padding));
}

// [[Rcpp::export]]
IntegerVector cpp_ntype(TokensPtr xptr, bool no_padding = false) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_ntype()\n";
#endif
    return Rcpp::wrap(xptr->get_ntype(no_padding));
}

// [[Rcpp::export]]
IntegerVector cpp_get_freq(TokensPtr xptr, bool no_padding = false,
                           bool boolean = false) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_get_freq()\n";
#endif
    
    std::vector<int> freq = xptr->get_freq(no_padding, boolean);
    IntegerVector freq_ = Rcpp::wrap(freq);
    CharacterVector types_ = encode(xptr->types);
    if (!no_padding)
        types_.push_front("");
    freq_.attr("names") = types_;
    return freq_;
}


// [[Rcpp::export]]
CharacterVector cpp_get_types(TokensPtr xptr) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_get_types()\n";
#endif
    return encode(xptr->types);
}

// [[Rcpp::export]]
int cpp_count_types(TokensPtr xptr) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_count_types()\n";
#endif
    return xptr->types.size();
}

// [[Rcpp::export]]
TokensPtr cpp_set_types(TokensPtr xptr, const CharacterVector types_) {
    Types types = Rcpp::as<Types>(types_);
    if (xptr->types != types) {
        xptr->types = types;
        xptr->recompiled = false;
    }
    return xptr;
}

// [[Rcpp::export]]
TokensPtr cpp_set_recompiled(TokensPtr xptr, bool recompiled) {
    xptr->recompiled = recompiled;
    return xptr;
}

// [[Rcpp::export]]
bool cpp_get_recompiled(TokensPtr xptr) {
    return xptr->recompiled;
}

// [[Rcpp::export]]
TokensPtr cpp_recompile(TokensPtr xptr, bool force = false) {

#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_recompile(force = " << force << ")\n";
#endif
    if (force)
        xptr->recompiled = false;
    xptr->recompile();
    return xptr;
}


/***R
require(quanteda)
toks <- tokens(c("b c b a,", "a b a d."), remove_punct = FALSE, padding = TRUE)
xtoks <- as.tokens_xptr(toks)
cpp_as_list(xtoks)
cpp_get_types(xtoks)
cpp_get_types(cpp_subset(xtoks, 1))
cpp_get_types(cpp_subset(xtoks, 2))
cpp_get_types(cpp_subset(xtoks, 1))
cpp_get_types(cpp_subset(xtoks, 2))
*/
