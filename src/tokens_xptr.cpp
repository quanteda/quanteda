#include "tokens.h"
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
    TokensObj *ptr_copy = new TokensObj(xptr->texts, xptr->types, xptr->has_gap, xptr->has_dup);
    return TokensPtr(ptr_copy, true);
}

// [[Rcpp::export]]
List cpp_get_attributes(TokensPtr xptr) {
    List list_ = List::create(_["has_dup"] = xptr->has_dup,
                              _["has_gap"] = xptr->has_gap);
    return list_;
}

// [[Rcpp::export]]
List cpp_as_list(TokensPtr xptr) {
    xptr->recompile();
    Tokens texts_ = as_list(xptr->texts);
    texts_.attr("types") = encode(xptr->types);;
    texts_.attr("padding") = xptr->has_pad;
    texts_.attr("class") = "tokens";
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
    xptr->texts = texts;
    return xptr;
}

// [[Rcpp::export]]
int cpp_ndoc(TokensPtr xptr) {
    return xptr->texts.size();
}


// [[Rcpp::export]]
IntegerVector cpp_ntoken(TokensPtr xptr) {
    //Rcout << "cpp_ntoken()\n";
    xptr->recompile();
    std::size_t H = xptr->texts.size();
    IntegerVector ls_(H);
    for (std::size_t h = 0; h < H; h++) {
        ls_[h] = xptr->texts[h].size();
    }
    return ls_;
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
    return xptr;
}

// [[Rcpp::export]]
void cpp_recompile(TokensPtr xptr) {
    xptr->recompile();
}

// [[Rcpp::export]]
S4 cpp_dfm(TokensPtr xptr) {
    
    xptr->recompile();
    std::size_t H = xptr->texts.size();
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
    int count_pad = 0;
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr->texts[h];
        std::sort(text.begin(), text.end()); // rows must be sorted in dgCMatrix
        int n = 1;
        for (std::size_t i = 0; i < text.size(); i++) {
            if (i + 1 == text.size() || text[i] != text[i + 1]) {
                if (text[i] == 0)
                    count_pad++;
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
    
    size_t G = xptr->types.size();
    CharacterVector types_ = encode(xptr->types);
    if (count_pad == 0) {
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
toks <- quanteda::tokens(c("a b a c.", "b c b a,"), remove_punct = FALSE, padding = TRUE)
xtoks <- quanteda::as.tokens_xptr(toks)
cpp_dfm(xtoks)
*/
