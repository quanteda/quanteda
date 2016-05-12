#include <Rcpp.h>
#include <vector>

using namespace Rcpp;


String join2(std::vector< std::string > ngram, 
            std::string delim){
  if(ngram.size() == 0) return "";
  String token_ngram = ngram[0];
  int len_ngram = ngram.size();
  for (int i = 1; i < len_ngram; i++) {
    token_ngram += delim;
    token_ngram += ngram[i];
    //Rcout << "Joined " << token_ngram.get_cstring()  << "\n";
  }
  token_ngram.set_encoding(CE_UTF8);
  return token_ngram;
}

// [[Rcpp::export]]
StringVector join_tokens_cpp(StringVector tokens_original, 
                             const std::vector<std::string> &tokens_join,
                             const std::string &delim){
  //StringVector tokens(x);
  StringVector tokens = clone(tokens_original);
  int len = tokens.size();
  int len_join = tokens_join.size();
  if(len_join == 1){ // Do nothing for single token
    return tokens ;
  }
  int start = -1;
  bool change = FALSE;
  String token_joined = join2(tokens_join, delim);
  for (int i = 0; i < len - (len_join - 1); i++){
    //Rcout << "Now " << i << " " << tokens[i] << "\n";
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
List join_tokens_cppl(List texts_original, 
                      const std::vector<bool> &flag,
                      const std::vector<std::string> &tokens_join,
                      const std::string &delim){
  //List texts(x);
  List texts = clone(texts_original);
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

