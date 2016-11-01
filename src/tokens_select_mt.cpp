#include <Rcpp.h>
#include <vector>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;
using namespace RcppParallel;

struct selecttokens : public Worker
{
    std::vector<CharacterVector> &input;
    std::vector<CharacterVector> &output;
    const std::unordered_set<String> set_types;
    const bool remove;
    const bool spacer;
    tthread::mutex lock_input;
    tthread::mutex lock_output;
    
    // Constructor
    selecttokens(std::vector<CharacterVector> &input, std::vector<CharacterVector> &output, const std::unordered_set<String> set_types, bool remove, bool spacer): 
        input(input), output(output), set_types(set_types), remove(remove), spacer(spacer){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            
            lock_input.lock();
            //Rcout << "H " << h << "\n";
            CharacterVector text = clone(input[h]);
            //Rcout << "Size " << text.size() << "\n";
            lock_input.unlock();
            
            CharacterVector text_temp(text.size()); // make vector in the same length as original
            int j = 0;
            for (int i = 0; i < text.size(); i++){
                String token = text[i];
                bool is_in = set_types.find(token) != set_types.end();
                if(is_in == remove){
                    //Rcout << "Match " << i << ' ' << token.get_cstring() << "\n";
                    if(spacer){
                        text_temp[j] = "";
                        j++;
                    }
                }else{
                    text_temp[j] = token;
                    j++;
                }
            }
            lock_output.lock();
            if(j == 0){
                output[h] = CharacterVector(); // nothing left
            }else{
                output[h] = text_temp[seq(0, j - 1)];
            }
            lock_output.unlock();
        }
    }
};

// [[Rcpp::export]]
List qatd_cpp_selecttokens_mt(SEXP x, 
                              CharacterVector &types,
                              const bool &remove,
                              const bool &spacer) {
    
    std::vector<CharacterVector> input = Rcpp::as< std::vector<CharacterVector> >(x);
    std::vector<CharacterVector> output(input.size());
    
    std::unordered_set<String> set_types;
    for (int g = 0; g < types.size(); g++){
        set_types.insert(types[g]);
    }
    //Rcout << "here\n";
    // call parallelFor to do the work
    selecttokens selecttokens(input, output, set_types, remove, spacer);
    
    // call parallelFor to do the work
    parallelFor(0, input.size(), selecttokens);
    
    return wrap(output);
}


/*** R
toks <- list(letters, LETTERS)
qatd_cpp_selecttokens_mt(rep(toks, 10), c('b', 'O', 'g', 'N'), FALSE, TRUE)
*/
