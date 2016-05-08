#include <Rcpp.h>
#include <set>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
//#include <unordered_set>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::string join(const std::vector<std::string> tokens, 
                 const std::string delim){
  
  if(tokens.size() == 0) return "";
  string token_joined = tokens[0];
  for(int i = 1; i < tokens.size(); ++i){
    //Rcout << "Join " << i << ' ' << token_joined << "\n";
    token_joined = token_joined + delim + tokens[i];
  }
  return token_joined;
}

// [[Rcpp::export]]
StringVector join_tokens_cpp(SEXP x, 
                             const vector<string> &tokens_join,
                             const string &delim){
  StringVector tokens(x);
  int len = tokens.size();
  int len_join = tokens_join.size();
  if(len_join == 1){ // Do nothing for single token
    return tokens ;
  }
  int start = -1;
  bool change = FALSE;
  String token_joined = join(tokens_join, delim);
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
        change = TRUE;
        start = -1; // Reset
      }else{
        if(tokens[i] != tokens_join[j]) start = -1; // Partial match
      }
    }
  }
  
  StringVector tokens_joined(len);
  int space = 0;
  if(change){
    //Rcout << "Remove spacer\n";
    int j = 0;
    for (int i = 0; i < len; i++){
      if(tokens[i] != ""){
        //tokens_joined.push_back(tokens[i]);
        tokens_joined[j] = tokens[i];
        j++;
      }
      
    }
    //Rcout << "Done\n";
    return tokens_joined[seq(0, j - 1)];
  }else{
    return tokens; // Return original if there is no change
  }
  
}

// [[Rcpp::export]]
List join_tokens_cppl(SEXP x, 
                      const vector< bool > &flag,
                      const vector< string > &tokens_join,
                      const string &delim){
  List texts(x);
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

