#include <Rcpp.h>
//#include <unordered_map>
//#include <numeric>
//#include "dev.h"
//#include "quanteda.hpp"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace std;
//using namespace quanteda;

typedef std::vector<unsigned int> Ngram;
typedef std::vector<unsigned int> Ngrams;


class ngramMaker {
  
public:
  ngramMaker(){}
  void skipgram_hashed(NumericVector tokens,
                         NumericVector ns, 
                         NumericVector skips,
                         std::unordered_map<Ngram, unsigned int> &map_ngram) {
    
      Rcout << "skipgram_hashed\n";
  }
  
  void qatd_cpp_ngram_hashed_vector(NumericVector tokens,
                                    NumericVector ns, 
                                    NumericVector skips){
    
      Rcout << "qatd_cpp_ngram_hashed_vector\n";
  }

};

RCPP_MODULE(ngram_module) {
  class_<ngramMaker>("ngramMaker")
  .constructor()
  .method("generate", &ngramMaker::qatd_cpp_ngram_hashed_list)
  ;
}



/*** R

# tokens <- tokenize(c('a b c d e', 'c d e f g'))
# tokens_hashed <- hashTokens(tokens)
# res <- qatd_cpp_ngram_hashed_list(tokens_hashed, 2, 0:1)
# res$text

#ngram_module <- Module("ngram_module")
#ngramMaker <- ngram_module$ngramMaker


nm <- new(ngramMaker)
res <- nm$generate(tokens_hashed)

# res$text
*/

