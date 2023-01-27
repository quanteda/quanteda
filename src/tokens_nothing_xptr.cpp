#include "lib.h"
#include "dev.h"
#include "recompile.h"
using namespace quanteda;

/* 
 * This function selects features in tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param text_ tokens ojbect
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * 
 */



// [[Rcpp::export]]
Xtokens qatd_cpp_nothing(Xtokens ptr,
                         const CharacterVector types_
){
    Texts texts = *ptr;
    for (std::size_t h = 0; h < ptr->size(); h++) {
        texts[h].push_back(0);
        ptr->at(h) =  texts[h];
    }
    return Xtokens(ptr, true);
}

// [[Rcpp::export]]
Xtokens qatd_cpp_as_xtokens(const List text_) {
    
    Texts texts = Rcpp::as<Texts>(text_);
    //std::vector<int> *text = &temp;
    Texts *ptr = new Texts(texts.size()) ;
    for (std::size_t h = 0; h < texts.size(); h++) {
      ptr->at(h) = texts[h];
    }
    //texts->push_back({1, 2, 3}) ;
    //texts->push_back({4, 5, 6}) ;
    //texts->at(0) = {7, 8};
    //Xtokens p(texts, true);
    return Xtokens(ptr, true);
}

// [[Rcpp::export]]
List qatd_cpp_as_tokens(Xtokens xptr) {
    Texts texts = *xptr;
    List texts_ = Rcpp::wrap(texts);
    return texts_;
}

/***R
toks <- rep(list(sample(100)), 100)
xtoks <- qatd_cpp_as_xtokens(toks)
xtoks <- qatd_cpp_nothing(xtoks, letters)
qatd_cpp_as_tokens(xtoks)

*/
