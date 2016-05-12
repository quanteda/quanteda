#include <Rcpp.h>
#include <set>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List select_tokens_cppl(Rcpp::List texts_original,
                              const std::vector< std::string > &types,
                              const bool &remove,
                              const bool &spacer){
  
  //Rcpp::List texts(x);
  Rcpp::List texts(texts_original.size());
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  int len_texts = texts_original.size();
  for (int h=0; h < len_texts; h++){
    //Rcout << "Text " << h << "\n";
    Rcpp::CharacterVector text_original = texts_original[h];
    Rcpp::CharacterVector text(text_original.size()); // make vector in the same length as original
    int j = 0;
    for (int i=0; i < text.size(); i++){
      Rcpp::String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << text[i] << "\n";
        if(spacer){
          text[j] = "";
          j++;
        }
      }else{
        text[j] = token;
        j++;
      }
    }
    if(j == 0){
      texts[h] = Rcpp::CharacterVector(); // nothing left
    }else{
      texts[h] = text[seq(0, j - 1)];
    }
  }
  return texts;
}

