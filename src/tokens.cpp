#include <Rcpp.h>
#include <set>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List select_tokens_cppl(SEXP x,  
                        std::vector< std::string > types, 
                        const bool &remove, 
                        const bool &spacer){
  
  Rcpp::List texts(x);
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
    texts[h] = text_temp;
  }
  return texts;
}


// [[Rcpp::export]]
std::string join(const std::vector<std::string> tokens, 
                 const std::string delim){
  
  if(tokens.size() == 0) return "";
  std::string token_joined = tokens[0];
  for(int i = 1; i < tokens.size(); ++i){
    //Rcout << "Join " << i << ' ' << token_joined << "\n";
    token_joined = token_joined + delim + tokens[i];
  }
  return token_joined;
}


// [[Rcpp::export]]
Rcpp::StringVector join_tokens_cpp(SEXP x, 
                                   const std::vector< std::string > &tokens_join,
                                   const std::string &delim){
  Rcpp::StringVector tokens(x);
  int len = tokens.size();
  int len_join = tokens_join.size();
  if(len_join == 1){ // Do nothing for single token
    return(tokens);
  }
  int start = -1;
  bool joined = FALSE;
  std::string token_joined = join(tokens_join, delim);
  for (int i = 0; i < len - (len_join - 1); i++){
    if(tokens[i] == tokens_join[0] & tokens[i + 1] == tokens_join[1]){ // Initial match
      start = i;
      //Rcout << "Start " << start << " " << tokens[i] << "\n";
    }
    if(start > -1){
      int j = i - start;
      if(j == len_join - 1){ // Complete match
        //Rcout << "End " << start << " " << tokens[i] << "\n";
        tokens[start] = token_joined;
        for(int k = start + 1; k < start + len_join; k++){
          //Rcout << "Remove " << k << ' ' << tokens[k] << "\n";
          tokens[k] = "";
        }
        joined = TRUE;
        start = -1; // Reset
      }else{
        if(tokens[i] != tokens_join[j]) start = -1; // Partial match
      }
    }
  }
  Rcpp::StringVector tokens_joined;
  if(joined){
    for (int i = 0; i < len; i++){
      if(tokens[i] != ""){
        tokens_joined.push_back(tokens[i]);
      }
    }
    return tokens_joined;
  }else{
    return tokens;
  }
  
}

// [[Rcpp::export]]
List join_tokens_cppl(SEXP x, 
                      const std::vector< bool > &flag,
                      const std::vector< std::string > &tokens_join,
                      const std::string &delim){
  Rcpp::List texts(x);
  int len = texts.size();
  if(flag.size() != len){
    Rcout << "Invalid flag is given\n";
    return texts;
  }
  for (int h = 0; h < len; h++){
    if(flag[h]){
      //Rcout << "Text " << h << "\n";
      texts[h] = join_tokens_cpp(texts[h], tokens_join, delim);
    }
  }
  return texts;
}
