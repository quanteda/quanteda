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
  const std::string space = "";
  int len_texts = texts.size();
  for (int h=0; h < len_texts; h++){
    std::vector< std::string > text = texts[h];
    std::vector< std::string > text_temp(text.size()); // make vector in the same length as original
    int j = 0;
    //int omit = 0;
    for (int i=0; i < text.size(); i++){
      Rcpp::String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << text_temp[i] << "\n";
        if(spacer){
          text_temp[j] = space;
          j++;
        }else{
          text_temp.pop_back(); // shorten the vector
        }
      }else{
        text_temp[j] = token;
        j++;
      }
    }
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}



// [[Rcpp::export]]
Rcpp::List select_tokens_cppl_loop(SEXP x,  
                              StringVector types, 
                              const bool &remove, 
                              const bool &spacer){
  
  Rcpp::List texts(x);
  //Rcpp::List texts_temp = clone(texts); // copy to keep original objects intact (uses more memory)
  Rcpp::List texts_temp(texts.size());
  
  bool is_in = FALSE;
  int len_texts = texts.size();
  for (int h=0; h < len_texts; h++){
    Rcpp::StringVector text = texts[h];
    Rcpp::StringVector text_temp;
    int len_text = text.size();
    for (int i=0; i < len_text; i++){
      Rcpp::String token = text[i];
      
      for(int j; j < types.size(); j++){
        if(token == types[j]){
          is_in = TRUE;
          break;
        };
      }
      if(is_in == remove){
        //Rcout << "Match " << i << ' ' << token << "\n";
        if(spacer){
          text_temp.push_back("");
        }
      }else{
        text_temp.push_back(token);
      }
    }
    //texts_temp.push_back(text_temp);
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}





