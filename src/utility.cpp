#include "lib.h"
using namespace quanteda;
//using namespace arma;

/* 
 * This function removes a string from of a list of string vectors.
 * @used tokens()
 * @creator Kohei Watanabe
 * @param input_ list of string vectors
 * @param char_remove a string to remove
 */

/*
// [[Rcpp::export]]
List cpp_chars_remove(List input_, String char_remove) {
    List output_ = clone(input_);
    for (unsigned int h = 0; h < (unsigned int)output_.size(); h++) {
        CharacterVector elems_ = output_[h];
        CharacterVector elems_new_(elems_.size());
        size_t j = 0;
        for (unsigned int i = 0; i < (unsigned int)elems_.size(); i++) {
            if(elems_[i] != char_remove) {
                elems_new_[j] = elems_[i];
                j++;
            }
        }
        if (j > 0){
            output_[h] = elems_new_[seq(0, j - 1)];
        }else{
            output_[h] = CharacterVector(0);
        }
    }
    return output_;
}
*/
 
/* 
 * This function checks values in values_ is uniform within groups specified by groups_
 * @used dfm_group()
 * @creator Kohei Watanabe
 * @param values_ a numeric vector for values
 * @param groups_ a integer vector for groups
 */

// [[Rcpp::export]]
bool cpp_is_grouped_numeric(NumericVector values_, IntegerVector groups_) {
    
    if (values_.size() == 0) return(true);
    if (min(groups_) < 1 || values_.size() != groups_.size())
        throw std::range_error("Invalid groups");
    
    unsigned int n = max(groups_);
    LogicalVector init_(n);
    CharacterVector values_init_(n);
    for (unsigned int i = 0; i < (unsigned int)groups_.size(); i++) {
        unsigned int g = groups_[i] - 1;
        if (!init_[g]) {
            init_[g] = true;
            values_init_[g] = values_[i];
        } else {
            if (values_init_[g] != values_[i]) {
                return(false);
            }
        }
    }
    return(true);
}

// [[Rcpp::export]]
bool cpp_is_grouped_character(CharacterVector values_, IntegerVector groups_) {
    
    if (values_.size() == 0) return(true);
    if (min(groups_) < 1 || values_.size() != groups_.size())
        throw std::range_error("Invalid groups");
    
    unsigned int n = max(groups_);
    LogicalVector init_(n);
    CharacterVector values_init_(n);
    for (unsigned int i = 0; i < (unsigned int)groups_.size(); i++) {
        unsigned int g = groups_[i] - 1;
        if (!init_[g]) {
            init_[g] = true;
            values_init_[g] = values_[i];
        } else {
            if (values_init_[g] != values_[i]) {
                return(false);
            }
        }
    }
    return(true);
}

/* 
 * Internal function to get max_load_factors of hash tables used for pattern
 * matching and ngram generation. Smaller values will increase the speed but also 
 * the use of RAM. Ngram generation requires significantly larger amount of
 * storage than pattern matching, so should be larger.
 * See: https://en.cppreference.com/w/cpp/container/unordered_map/max_load_factor
 * @param pattern or ngrams
 * @param value between 0 and 1.0
 */
//
// [[Rcpp::export]]
List cpp_get_load_factor() {
    return List::create(
         _["pattern"] = GLOBAL_PATTERN_MAX_LOAD_FACTOR,
         _["ngrams"] = GLOBAL_NGRAMS_MAX_LOAD_FACTOR
    );
}

// Copied from the pryr package
// [[Rcpp::export]]
std::string address(SEXP x) {
    std::ostringstream s;
    s << x;
    return s.str();
}

// [[Rcpp::export]]
void cpp_set_meta(RObject object_, RObject meta_) {
    object_.attr("meta") = meta_;
}

// [[Rcpp::export]]
int cpp_get_max_thread() {
#if QUANTEDA_USE_TBB
    return tbb::this_task_arena::max_concurrency();
#else
    return 1;
#endif 
}

// [[Rcpp::export]]
bool cpp_tbb_enabled(){
#if QUANTEDA_USE_TBB
    return true;
#else
    return false;
#endif
}
