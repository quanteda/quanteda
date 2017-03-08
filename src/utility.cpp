#include "quanteda.h"
using namespace quanteda;

/* 
 * This funciton removes a string from of a list of string vectors.
 * @used tokens()
 * @creator Kohei Watanabe
 * @param input_ list of string vectors
 * @param char_remove a string to remove
 */

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


/*** R

*/
