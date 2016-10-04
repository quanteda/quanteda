#include <Rcpp.h>
#include <vector>
#include <mutex>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;
using namespace RcppParallel;
std::mutex lock_input3;
std::mutex lock_output3;

struct select_tokens3 : public Worker
{
  std::vector<CharacterVector> &input;
  std::vector<CharacterVector> &output;
  const std::unordered_set<String> set_types;
  const bool remove;
  const bool spacer;
  
  // Constructor
  select_tokens3(std::vector<CharacterVector> &input, std::vector<CharacterVector> &output, const std::unordered_set<String> set_types, bool remove, bool spacer): 
                input(input), output(output), set_types(set_types), remove(remove), spacer(spacer){}
  
  // parallelFor calles this function with size_t
  void operator()(std::size_t begin, std::size_t end){
    
    std::vector<CharacterVector> texts(end - begin + 1);
    int f = 0;
    lock_input3.lock();
    //Rcout << "Range " << begin << " " << end << "\n";
    for (int g = begin; g < end + 1; g++){ 
      texts[f] = input[g];
      f++;
    }
    lock_input3.unlock();
    
    return;
    for (int h = 0; h < texts.size(); h++){
      //Rcout << "H " << h << "\n";
      CharacterVector text = texts[h];
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
      
      if(j == 0){
        output[h] = CharacterVector(); // nothing left
      }else{
        output[h] = text_temp[seq(0, j - 1)];
      }
    }
  }
};

// [[Rcpp::export]]
List select_tokens_cppl_mt3(SEXP x, 
                             CharacterVector &types,
                             const bool &remove,
                             const bool &spacer) {
  
  std::vector<CharacterVector> input = Rcpp::as< std::vector<CharacterVector> >(x);
  std::vector<CharacterVector> output(input.size());
  
  std::unordered_set<String> set_types;
  for (int g = 0; g < types.size(); g++){
    set_types.insert(types[g]);
  }
  
  // SquareRoot functor (pass input and output matrixes)
  select_tokens3 select_tokens3(input, output, set_types, remove, spacer);
  
  // call parallelFor to do the work
  parallelFor(0, input.size(), select_tokens3);
  
  return wrap(output);
}


/*** R

toks <- list(letters, LETTERS)
#select_tokens_cppl_mt(toks, c('b', 'O', 'g', 'N'), TRUE, TRUE)
#select_tokens_cppl_mt(rep(toks, 1000), c('b', 'O', 'g', 'N'), FALSE, TRUE)

*/
