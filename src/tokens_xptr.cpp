#include "lib.h"
#include "dev.h"
//#include "recompile.h"
using namespace quanteda;

// [[Rcpp::export]]
TokensPtr cpp_as_xptr(const List text_, 
                      const CharacterVector types_) {
    
    Texts texts = Rcpp::as<Texts>(text_);
    Types types = Rcpp::as<Types>(types_);
    TokensObj *ptr = new TokensObj(texts, types);
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
    xptr->recompile();
    List texts_ = as_list(xptr->texts);
    texts_.attr("types") = encode(xptr->types);
    texts_.attr("class") = "tokens";
    return texts_;
}

// [[Rcpp::export]]
List cpp_as_list(TokensPtr xptr) {
    Tokens texts_ = as_list(xptr->texts);
    texts_.attr("types") = encode(xptr->types);;
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
    xptr->recompile();
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
    xptr->recompile();
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
    xptr->recompile();
    std::size_t G = xptr->types.size();
    if (!no_padding)
        G++;
    IntegerVector freq_(G, 0);
    std::size_t H = xptr->texts.size();
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr->texts[h];
        std::size_t I = text.size();
        std::vector<bool> flag(G);
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
            freq_[id]++;
            flag[id] = true;
        }
    }
    CharacterVector types_ = encode(xptr->types);
    if (!no_padding)
        types_.push_front("");
    freq_.attr("names") = types_;
    return freq_;
}


// [[Rcpp::export]]
CharacterVector cpp_get_types(TokensPtr xptr, bool recompile = false) {
    //Rcout << "cpp_types()\n";
    if (recompile)
        xptr->recompile();
    return encode(xptr->types);
}

// [[Rcpp::export]]
TokensPtr cpp_set_types(TokensPtr xptr, const CharacterVector types_) {
    Types types = Rcpp::as<Types>(types_);
    xptr->types = types;
    xptr->recompiled = false;
    return xptr;
}

// [[Rcpp::export]]
void cpp_recompile(TokensPtr xptr) {
    xptr->recompile();
}

// [[Rcpp::export]]
S4 cpp_dfm(TokensPtr xptr, bool asis = true) {
    
    xptr->recompiled = asis;
    xptr->recompile();
    std::size_t H = xptr->texts.size();
    std::size_t G = xptr->types.size();

    int N = 0;
    for (std::size_t h = 0; h < H; h++)
        N += xptr->texts[h].size();
    std::vector<double> slot_x;
    std::vector<int> slot_i, slot_p;
    slot_i.reserve(N);
    slot_x.reserve(N);
    slot_p.reserve(H + 1);
    int p = 0;
    
    slot_p.push_back(p);
    for (std::size_t h = 0; h < H; h++) {
        // assign new token IDs in the order of their occurrence
        Text text = xptr->texts[h];
        std::size_t I = text.size();

        // aggregate the same token IDs
        std::sort(text.begin(), text.end()); // rows must be sorted in dgCMatrix
        int n = 1;
        for (std::size_t i = 0; i < I; i++) {
            if (i + 1 == text.size() || text[i] != text[i + 1]) {
                slot_i.push_back(text[i]);
                slot_x.push_back(n);
                p++;
                n = 1;
            } else {
                n++;
            }
        }
        slot_p.push_back(p);
    }
    IntegerVector slot_p_ = Rcpp::wrap(slot_p);
    //Rcout << "p: " << slot_p_ << "\n";
    DoubleVector slot_x_ = Rcpp::wrap(slot_x);
    //Rcout << "x: " << slot_x_ << "\n";
    IntegerVector slot_i_ = Rcpp::wrap(slot_i);
    //Rcout << "i: " << slot_i_ << "\n";
    CharacterVector types_ = encode(xptr->types);
    
    //Rcout << "types: " << types_ << "\n";
    
    if (!xptr->padded) {
        slot_i_ = slot_i_ - 1; // use zero for other tokens
    } else {
        G++;
        types_.push_front("");
    }
    
    IntegerVector dim_ = IntegerVector::create(G, H);
    List dimnames_ = List::create(types_, R_NilValue);
    
    S4 dfm_("dgCMatrix");
    dfm_.slot("p") = slot_p_;
    dfm_.slot("i") = slot_i_;
    dfm_.slot("x") = slot_x_;
    dfm_.slot("Dim") = dim_;
    dfm_.slot("Dimnames") = dimnames_;
    return(dfm_);
}


/***R
require(quanteda)
toks <- tokens(c("b c b a,", "a b a c."), remove_punct = FALSE, padding = TRUE)
xtoks <- as.tokens_xptr(toks)
xtoks_dict <- tokens_lookup(xtoks, dictionary(list(A = "a", Z = "z", B = "b")))
#xtoks_dict <- tokens_lookup(xtoks, dictionary(list(A = "a", B = "b")))
quanteda:::cpp_get_attributes(xtoks_dict)
quanteda:::cpp_get_types(xtoks_dict)
print(xtoks_dict)
cpp_dfm(as.tokens_xptr(xtoks_dict), FALSE)
cpp_dfm(as.tokens_xptr(xtoks_dict), TRUE)


*/
