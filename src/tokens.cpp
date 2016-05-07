#include <Rcpp.h>
#include <set>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List select_tokens_cppl(SEXP x,
                               const std::vector< std::string > &types,
                               const bool &remove,
                               const bool &spacer){
  
  Rcpp::List texts(x);
  Rcpp::List texts_temp(texts.size());
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  int len_texts = texts.size();
  for (int h=0; h < len_texts; h++){
    //Rcout << "Text " << h << "\n";
    Rcpp::CharacterVector text = texts[h];
    Rcpp::CharacterVector text_temp(text.size()); // make vector in the same length as original
    int j = 0;
    for (int i=0; i < text.size(); i++){
      Rcpp::String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << text_temp[i] << "\n";
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
      texts_temp[h] = Rcpp::CharacterVector(); // nothing left
    }else{
      texts_temp[h] = text_temp[seq(0, j - 1)];
    }
  }
  return texts_temp;
}

