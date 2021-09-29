#include "lib.h"
//#include "dev.h"
#include "recompile.h"
using namespace quanteda;

struct shift_mt : public Worker{
    
    Texts &texts;
    const int shift;

    // Constructor
    shift_mt(Texts &texts_, const int shift_):
             texts(texts_), shift(shift_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                if (texts[h][i] > 0)
                    texts[h][i] = texts[h][i] + shift;
            }
        }
    }
};

/* 
 * This function selects features in tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_combine(const List &texts1_, 
                             const CharacterVector types1_,
                             const List &texts2_, 
                             const CharacterVector types2_){
    
    Texts texts1 = Rcpp::as<Texts>(texts1_);
    Types types1 = Rcpp::as<Types>(types1_);
    Texts texts2 = Rcpp::as<Texts>(texts2_);
    Types types2 = Rcpp::as<Types>(types2_);
    const int shift = types1.size();
    
    // dev::Timer timer;
    // dev::start_timer("Token shift", timer);
#if QUANTEDA_USE_TBB
    shift_mt shift_mt(texts2, shift);
    parallelFor(0, texts2.size(), shift_mt);
#else
    for (std::size_t h = 0; h < texts2.size(); h++) {
        for (std::size_t i = 0; i < texts2[h].size(); i++) {
            if (texts2[h][i] > 0)
                texts2[h][i] = texts2[h][i] + shift;
        }
    }
#endif
    
    Texts texts;
    texts.reserve(texts1.size() + texts2.size());
    texts.insert(texts.end(), texts1.begin(), texts1.end());
    texts.insert(texts.end(), texts2.begin(), texts2.end());
    
    Types types;
    types.reserve(types1.size() + types2.size());
    types.insert(types.end(), types1.begin(), types1.end());
    types.insert(types.end(), types2.begin(), types2.end());
    
    // dev::stop_timer("Token shift", timer);
    return recompile(texts, types, false, true, is_encoded(types1_) || is_encoded(types2_));
}

/***R
toks1 <- list(rep(1:10, 1))
toks2 <- list(rep(1:15, 1))
unclass(qatd_cpp_tokens_combine(toks, letters[1:10], toks2, letters[1:15]))
*/
