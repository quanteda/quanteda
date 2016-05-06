#include <Rcpp.h>
#include <set>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List select_tokens_cppl(SEXP x,  
                        std::vector< std::string > types, 
                        const bool &remove, 
                        const bool &spacer){
  
  Rcpp::List texts(x);
  Rcpp::List texts_temp = clone(texts); // copy to keep original objects intact (uses more memory)
  std::set<std::string> set_types (types.begin(), types.end());
  
  int len_texts = texts.size();
  for (int h=0; h < len_texts; h++){
    Rcpp::StringVector text = texts[h];
    Rcpp::StringVector text_temp;
    int len_text = text.size();
    for (int i=0; i < len_text; i++){
      Rcpp::String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << token << "\n";
        if(spacer){
          text_temp.push_back("");
        }
      }else{
        text_temp.push_back(token);
      }
    }
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}


