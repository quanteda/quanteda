#include "lib.h"
//#include "dev.h"
using namespace quanteda;

// [[Rcpp::export]]
S4 cpp_dfm(TokensPtr xptr) {
    
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
    slot_p.clear();
    //Rcout << "p: " << slot_p_ << "\n";
    DoubleVector slot_x_ = Rcpp::wrap(slot_x);
    slot_x.clear();
    //Rcout << "x: " << slot_x_ << "\n";
    IntegerVector slot_i_ = Rcpp::wrap(slot_i);
    slot_i.clear();
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

// [[Rcpp::export]]
IntegerMatrix cpp_as_matrix(TokensPtr xptr, std::size_t length, 
                            const IntegerVector extract_, bool asis = true) {
    
    xptr->recompiled = asis;
    xptr->recompile();
    
    std::size_t G = extract_.size();
    std::size_t H = xptr->texts.size();
    IntegerVector vec_(G * length, 0); 
    for (std::size_t g = 0; g < G; g++) {
        if (extract_[g] < 0 || (int)H < extract_[g])
            throw std::range_error("Invalid document index");
        std::size_t h = extract_[g] - 1;
        Text text = xptr->texts[h];
        for (std::size_t i = 0; i < length; i++) {
            if (i < text.size())
                vec_[g + (G * i)] = text[i];
        }
    }
    IntegerMatrix mat_(G, length, vec_.begin());
    return(mat_);
}
