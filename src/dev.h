#ifndef __DEV__
#define __DEV__

using namespace Rcpp;
using namespace std;

namespace dev{

    inline void print_ngram_hashed(std::vector<unsigned int> vec){
        for(auto elm : vec){
          Rcout << to_string(elm) << " ";
        }
          Rcout << "\n";
    }
    
    inline void print_ngram(std::vector<std::string> vec){
        for(auto elm : vec){
          Rcout << elm << " ";
        }
        Rcout << "\n";
    }

}

#endif