#include <Rcpp.h>
#include <string>
//#include <boost/unordered_set.hpp>
// [[Rcpp::plugins(cpp11)]]
#include <map> 
#include <unordered_map> 
//#include <algorithm>

//using namespace boost;
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
void index_cpp(Rcpp::List &texts,
               const CharacterVector &types,
               IntegerVector &index_d,
               IntegerVector &index_f){
                     
  
  Rcout << "Assigning IDs\n";
  
  // Assign integer IDs
  std::unordered_map<String, int> index(types.size());
  for (int g = 0; g < types.size(); g++){
    index[types[g]] = g + 1;
  }
  
  Rcout << "Converting to IDs\n";
  // Convert tokens to IDs
  int k = 0;
  for (int h = 0; h < texts.size(); h++){
    StringVector tokens = texts[h];
    //Rcout << "Text " <<  h << "\n";
    for (int i = 0; i < tokens.size(); i++){
      //Rcout << "Token " <<  tokens[i] << "\n";
      index_d[k] = h + 1;
      index_f[k] = index[tokens[i]];
      k++;
    }
  }
  Rcout << "Indexing completed\n";
}


