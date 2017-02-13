#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
// to remove a character from the list
// [[Rcpp::export]]
List qatd_cpp_chars_remove(List input_, String char_remove){
    List output_ = clone(input_);
    for (unsigned int h = 0; h < (unsigned int)output_.size(); h++){
        CharacterVector elems = output_[h];
        CharacterVector elems_new(elems.size());
        size_t j = 0;
        for (unsigned int i = 0; i < (unsigned int)elems.size(); i++){
            if(elems[i] != char_remove){
                elems_new[j] = elems[i];
                j++;
            }
        }
        if (j > 0){
            output_[h] = elems_new[seq(0, j - 1)];
        }else{
            output_[h] = CharacterVector(0);
        }
    }
    return output_;
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
