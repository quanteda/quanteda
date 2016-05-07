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
std::vector<int> index_cpp_std(std::vector< std::string > tokens, 
                                    std::vector< std::string > types){
  
  
  std::map<string, int> index;
  
  // Assign integer IDs
  for (int h = 0; h < types.size(); h++){
    std::string type = types[h];
    index[type] = h + 1;
    //index["aaaaa"] = h + 1;
    //index.insert(std::pair<string,int>(type, h));
  }
  //Rcout << "Types " <<  index.size() << "\n";
  
  // Convert tokens to IDs
  std::vector<int> indices;
  for (int i = 0; i < tokens.size(); i++){
    std::string token = tokens[i];
    //Rcout << "Token " <<  token << " " << index[token] << "\n";
    indices.push_back(index[token]);
  }
  return indices;
}


// [[Rcpp::export]]
NumericVector index_cpp_std_umap(const std::vector< std::string > &tokens, 
                                 const std::vector< std::string > &types){
  
  std::unordered_map<string, int> index;
  
  // Assign integer IDs
  for (int h = 0; h < types.size(); h++){
    index[types[h]] = h + 1;
  }

  // Convert tokens to IDs
  std::vector<int> indices(tokens.size());
  for (int i = 0; i < tokens.size(); i++){
    //Rcout << "Token " <<  token << " " << index[token] << "\n";
    indices[i] = index[tokens[i]];
  }
  return wrap(indices);
}

// [[Rcpp::export]]
std::vector<int> index_cpp_boost(std::vector<std::string> tokens, std::vector<std::string> types) {
  
  typedef unordered_map<std::string, int> unordered_map;
  unordered_map map;

  for (int h = 0; h < types.size(); h++){
    std::string type = types[h];
    map.emplace(type, h + 1);
  }
  std::vector<int> indices;
  for (int i = 0; i < tokens.size(); i++){
    std::string token = tokens[i];
    //Rcout << "Token " <<  map[token] << "\n";
    indices.push_back(map[token]);
  }
  return indices;

}


// [[Rcpp::export]]
NumericVector index_cpp_r(StringVector tokens, 
                         StringVector types){

  // Convert tokens to IDs
  NumericVector indices;
  for (int i = 0; i < tokens.size(); i++){
    for (int j = 0; j < types.size(); j++){
      if(tokens[i] == types[j]){
        indices.push_back(j);
        break;
      }
    }
  }
  return indices;
}




