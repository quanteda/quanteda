#include <Rcpp.h>
#include <vector>
#include <mutex>
//#include <algorithm>    // std::transform

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;
using namespace RcppParallel;

struct select_tokens5 : public Worker
{
  std::vector< std::vector<int> > &input;
  std::vector< std::vector<int> > &output;
  const std::unordered_set<int> set_types;
  const bool remove;
  const bool spacer;
  //std::mutex lock_input5;
  //std::mutex lock_output5;
  
  tthread::mutex lock_input5;
  tthread::mutex lock_output5;
  
  // Constructor
  select_tokens5(std::vector< std::vector<int> > &input, std::vector< std::vector<int> > &output, const std::unordered_set<int> set_types, bool remove, bool spacer): 
                input(input), output(output), set_types(set_types), remove(remove), spacer(spacer){}
  
  // parallelFor calles this function with size_t
  void operator()(std::size_t begin, std::size_t end){
    //Rcout << "Range " << begin << " " << end << "\n";
    for (int h = begin; h < end; h++){
      
      //lock_input5.lock();
      //Rcout << "H " << h << "\n";
      std::vector<int> text = input[h];
      //Rcout << "Size " << text.size() << "\n";
      //lock_input5.unlock();
      
      std::vector<int> text_temp(text.size());// make vector in the same length as original
      
      int j = 0;
      for (int i = 0; i < text.size(); i++){
        int token = text[i];
        bool is_in = set_types.find(token) != set_types.end();
        if(is_in == remove){
          
          if(spacer){
            text_temp[j] = 0;
            j++;
          }
        }else{
          text_temp[j] = token;
          j++;
        }
      }

      //lock_output5.lock();
      if(j == 0){
        output[h] = std::vector<int>(); // nothing left
      }else{
        text_temp.resize(j);
        output[h] = text_temp;
      }
      //Rcout << "J " << j << "\n";
      //lock_output5.unlock();

    }
  }
};

// [[Rcpp::export]]
List select_tokens_cppl_mt5(SEXP x, 
                           std::vector<int> &types,
                           const bool &remove,
                           const bool &spacer) {
  
  std::vector< std::vector<int> > input = Rcpp::as< std::vector< std::vector<int> > >(x);
  std::vector< std::vector<int> > output(input.size());
  
   //std::vector<int> types_hash(types.size());
  //std::transform (types.begin(), types.end(), types_hash.begin(), hash);
  //std::unordered_set<int> set_types (types_hash.begin(), types_hash.end());
  
  std::unordered_set<int> set_types (types.begin(), types.end());
  select_tokens5 select_tokens5(input, output, set_types, remove, spacer);
  
  // call parallelFor to do the work
  parallelFor(0, input.size(), select_tokens5);
  
  // return the output matrix
  return wrap(output);
}


/*** R

toks <- list(1:50, 200:250)
targets <- c(10, 50, 220, 230)
microbenchmark::microbenchmark(
out <-  select_tokens_cppl_mt5(rep(toks, 1000000), targets, FALSE, TRUE),
times=10)
*/
