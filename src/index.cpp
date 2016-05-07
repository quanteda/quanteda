#include <Rcpp.h>
#include <string>
//#include <boost/unordered_set.hpp>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_map> 
//#include <algorithm>

//using namespace boost;
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
Rcpp::List index_cpp(SEXP x, 
                      const std::vector< std::string > &types,
                      const int &n){
  
  Rcpp::List texts(x);
  std::unordered_map<String, int> index;
  
  // Assign integer IDs
  for (int g = 0; g < types.size(); g++){
    index[types[g]] = g + 1;
  }

  // Convert tokens to IDs
  NumericVector idx_doc(n);
  NumericVector idx_feat(n);
  int k = 0;
  for (int h = 0; h < texts.size(); h++){
    StringVector tokens = texts[h];
    //Rcout << "Text " <<  h << "\n";
    for (int i = 0; i < tokens.size(); i++){
      //Rcout << "Token " <<  tokens[i] << "\n";
      if (index.find(tokens[i]) != index.end()){
        idx_doc[k] = h + 1;
        idx_feat[k] = index[tokens[i]];
        k++;
      }
    }
  }
  return List::create(Named("doc") = idx_doc , Named("feature") = idx_feat);
}


