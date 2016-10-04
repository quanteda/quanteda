#include <Rcpp.h>
#include <vector>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;
using namespace RcppParallel;

struct selecttokens_hashed : public Worker
{
  std::vector< std::vector<int> > &input;
  std::vector< std::vector<int> > &output;
  const std::unordered_set<int> set_types;
  const bool remove;
  const bool spacer;
  tthread::mutex lock_input;
  tthread::mutex lock_output;
  
  // Constructor
  selecttokens_hashed(std::vector< std::vector<int> > &input, std::vector< std::vector<int> > &output, const std::unordered_set<int> set_types, bool remove, bool spacer): 
                      input(input), output(output), set_types(set_types), remove(remove), spacer(spacer){}
  
  // parallelFor calles this function with size_t
  void operator()(std::size_t begin, std::size_t end){
    //Rcout << "Range " << begin << " " << end << "\n";
    for (int h = begin; h < end; h++){
      
      lock_input.lock();
      //Rcout << "H " << h << "\n";
      std::vector<int> text = input[h];
      //Rcout << "Size " << text.size() << "\n";
      lock_input.unlock();
      
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

      lock_output.lock();
      if(j == 0){
        output[h] = std::vector<int>(); // nothing left
      }else{
        text_temp.resize(j);
        output[h] = text_temp;
      }
      //Rcout << "J " << j << "\n";
      lock_output.unlock();

    }
  }
};

// [[Rcpp::export]]
List qatd_cpp_selecttokens_mt_hashed(SEXP x, 
                                     std::vector<int> &types,
                                     const bool &remove,
                                     const bool &spacer) {
  
  std::vector< std::vector<int> > input = Rcpp::as< std::vector< std::vector<int> > >(x);
  std::vector< std::vector<int> > output(input.size());

  std::unordered_set<int> set_types (types.begin(), types.end());
  selecttokens_hashed selecttokens_hashed(input, output, set_types, remove, spacer);
  
  // call parallelFor to do the work
  parallelFor(0, input.size(), selecttokens_hashed);
  
  // return the output matrix
  return wrap(output);
}


/*** R

toks <- list(1:50, 200:250)
targets <- c(10, 50, 220, 230)
microbenchmark::microbenchmark(
out <-  qatd_cpp_selecttokens_mt_hashed(rep(toks, 10000), targets, FALSE, TRUE),
times=10)
*/
