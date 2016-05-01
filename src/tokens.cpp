#include <Rcpp.h>
#include <set>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List select_tokens_cpp(SEXP x,  std::vector< std::string > types, bool remove, bool spacer){
  
  Rcpp::List texts(x);
  std::set<std::string> set_types (types.begin(), types.end());
  
  int len_texts = texts.size();
  for (int h=0; h < len_texts; h++){
    std::vector<std::string> text = texts[h];
    std::vector<std::string> text_temp;
    int len_text = text.size();
    for (int i=0; i < len_text; i++){
      std::string token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << token << "\n";
        if(spacer){
          text_temp.push_back('\0');
        }
      }else{
        text_temp.push_back(token);
      }
    }
    texts[h] = text_temp;
  }
  return texts;
}

