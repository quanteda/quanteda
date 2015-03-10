#include <Rcpp.h>
#include <string.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
Rcpp::CharacterVector justTokenizeCpp(SEXP x, SEXP sep, SEXP minLength) {
  
    std::string str = Rcpp::as <string> (x); 
    std::string delim = Rcpp::as <string> (sep);
    const char *delim_char = delim.c_str();
    int len_min = Rcpp::as <int> (minLength);
    
    int len_str = str.length();
    int i_pos = 0;
    int i_len = 0;
    bool flag_token = false;
    std::string token;
    std::vector <std::string> tokens;
    
    for(int i=0; i <= len_str; i++){
        i_len = i - i_pos;
        if(str[i] == delim_char[0] || i == len_str ){
            token = str.substr(i_pos, i_len);
            
            if( flag_token) {
                if (i_len >= len_min) {
                    tokens.push_back(token);
                }
                flag_token = false;
            }
        } else {
            if( !flag_token) {
                i_pos = i;
                flag_token = true;
            }
        }
    }    
  return wrap(tokens);
}
