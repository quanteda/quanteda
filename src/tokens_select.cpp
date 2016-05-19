#include <Rcpp.h>
#include <set>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// [[Rcpp::export]]
void select_tokens_cppl(Rcpp::List texts,
                              const std::vector< std::string > &types,
                              const bool &remove,
                              const bool &spacer){
  
  //Rcpp::List texts(x);
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  for (int h=0; h < texts.size(); h++){
    //Rcout << "Text " << h << "\n";
    Rcpp::CharacterVector text = texts[h];
    Rcpp::CharacterVector text_temp(text.size()); // make vector in the same length as original
    int j = 0;
    for (int i=0; i < text_temp.size(); i++){
      Rcpp::String token = text[i];
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
      texts[h] = Rcpp::CharacterVector(); // nothing left
    }else{
      texts[h] = text_temp[seq(0, j - 1)];
    }
  }
}

