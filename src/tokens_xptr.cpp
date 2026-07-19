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
    
    std::size_t H = xptr->texts.size();
    IntegerVector ls_(H, 0);
    if (no_padding) {
        for (std::size_t h = 0; h < H; h++) {
            std::size_t I = xptr->texts[h].size();
            for (std::size_t i = 0; i < I; i++) {
                // ignore paddings
                if (xptr->texts[h][i] > 0)
                    ls_[h]++;
            }
        }
    } else {
        for (std::size_t h = 0; h < H; h++) {
            ls_[h] = xptr->texts[h].size();
        }
    }
    return ls_;
}

// [[Rcpp::export]]
IntegerVector cpp_ntype(TokensPtr xptr, bool no_padding = false) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_ntype()\n";
#endif
    
    std::size_t H = xptr->texts.size();
    IntegerVector ns_(H);
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr->texts[h];
        if (text.size() == 0) {
            ns_[h] = 0;
        } else {
            std::sort(text.begin(), text.end());
            text.erase(unique(text.begin(), text.end()), text.end());
            int n = text.size();
            if (text[0] == 0 && no_padding)
                n--;
            ns_[h] = n;
        }
    }
    return ns_;
}

// [[Rcpp::export]]
IntegerVector cpp_get_freq(TokensPtr xptr, bool no_padding = false,
                           bool boolean = false) {
    
#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_get_freq()\n";
#endif
    
    std::size_t G = xptr->types.size();
    if (!no_padding)
        G++;
    std::vector<int> freq(G, 0);
    std::vector<bool> flag(G);
    std::size_t H = xptr->texts.size();
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr->texts[h];
        std::fill(flag.begin(), flag.end(), false);
        std::size_t I = text.size();
        for (std::size_t i = 0; i < I; i++) {
            unsigned int id = text[i];
            // ignore paddings
            if (no_padding) {
                if (id == 0)
                    continue;
                id--;
            }
            if (boolean && flag[id])
                continue;
            freq[id]++;
            flag[id] = true;
        }
    }
    IntegerVector freq_ = Rcpp::wrap(freq);
    CharacterVector types_ = encode(xptr->types);
    if (!no_padding)
        types_.push_front("");
    freq_.attr("names") = types_;
    return freq_;
}


// [[Rcpp::export]]
CharacterVector cpp_get_types(TokensPtr xptr, bool all = true) {
    
    #ifdef QUANTEDA_DEBUG
        Rcout << "call cpp_get_types()\n";
    #endif
    
    // if (recompile)
    //     xptr->recompile();
    // return encode(xptr->types);
    
    if (all)
        return encode(xptr->types);
    
    std::size_t G = xptr->types.size();
    std::vector<bool> flag(G);
    std::vector<std::string> types;
    types.reserve(G);
    
    std::size_t H = xptr->texts.size();
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr->texts[h];
        std::size_t I = text.size();
        for (std::size_t i = 0; i < I; i++) {
            unsigned int id = text[i];
            if (id != 0)
                flag[id - 1] = true;
        }
    }
    for (std::size_t g = 0; g < G; g++) {
        if (flag[g])
            types.push_back(xptr->types[g]);
    }
    return encode(types);
}

// [[Rcpp::export]]
TokensPtr cpp_set_types(TokensPtr xptr, const CharacterVector types_) {
    Types types = Rcpp::as<Types>(types_);
    xptr->types = types;
    xptr->recompiled = false;
    return xptr;
}

// [[Rcpp::export]]
TokensPtr cpp_recompile(TokensPtr xptr, bool force = false) {

#ifdef QUANTEDA_DEBUG
    Rcout << "call cpp_recompile(force = " << force << ")\n";
#endif
    if (force)
        xptr->recompiled = false;
    xptr->recompile();
    return(xptr);
}



/***R
require(quanteda)
toks <- tokens(c("b c b a,", "a b a d."), remove_punct = FALSE, padding = TRUE)
xtoks <- as.tokens_xptr(toks)
cpp_as_list(xtoks)
cpp_get_types(xtoks)
cpp_get_types(cpp_subset(xtoks, 1))
cpp_get_types(cpp_subset(xtoks, 2))
cpp_get_types(cpp_subset(xtoks, 1), all = TRUE)
cpp_get_types(cpp_subset(xtoks, 2), all = TRUE)
*/
