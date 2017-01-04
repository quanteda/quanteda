#include <Rcpp.h>
#include <vector>

using namespace Rcpp;


// [[Rcpp::export]]
SEXP qatd_cpp_deepcopy(SEXP x_){
    return clone(x_);
}
// [[Rcpp::export]]
List qatd_cpp_structcopy_int_list(List list_){
    // Generate empty List
    List list = list_;
    List list_struct;
    for (size_t g = 0; g < list.size(); g++){
        IntegerVector elems = list[g];
        list_struct.push_back(IntegerVector(elems.size()));
    }
    return list_struct;
}

// [[Rcpp::export]]
List qatd_cpp_remove_chr_list(List list_, String elem_remove){
    List list = clone(list_);
    for (size_t h = 0; h < list.size(); h++){
        CharacterVector elems = list[h];
        CharacterVector elems_new(elems.size());
        size_t j = 0;
        for (size_t i = 0; i < elems.size(); i++){
            if(elems[i] != elem_remove){
                elems_new[j] = elems[i];
                j++;
            }
        }
        if (j > 0){
            list[h] = elems_new[seq(0, j - 1)];
        }else{
            list[h] = CharacterVector(0);
        }
    }
    return list;
}

// [[Rcpp::export]]
List qatd_cpp_remove_int_list(List list_, int elem_remove){
    List list = clone(list_);
    for (size_t h = 0; h < list.size(); h++){
        IntegerVector elems = list[h];
        IntegerVector elems_new(elems.size());
        size_t j = 0;
        for (size_t i = 0; i < elems.size(); i++){
            if(elems[i] != elem_remove){
                elems_new[j] = elems[i];
                j++;
            }
        }
        if (j > 0){
            list[h] = elems_new[seq(0, j - 1)];
        }else{
            list[h] = IntegerVector(0);
        }
    }
    return list;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
list <- list(c(1:10), c(1:20))
qatd_cpp_structcopy_int_list(list)

# df <- expand.grid(LETTERS[1:3], LETTERS[4:5], LETTERS[6], stringsAsFactors=FALSE)
# qatd_cpp_split_df(df)
*/
