#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
// to remove a character from the list
// [[Rcpp::export]]
List qatd_cpp_remove_chr_list(List list_, String elem_remove){
    List list = clone(list_);
    for (unsigned int h = 0; h < list.size(); h++){
        CharacterVector elems = list[h];
        CharacterVector elems_new(elems.size());
        size_t j = 0;
        for (unsigned int i = 0; i < elems.size(); i++){
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
