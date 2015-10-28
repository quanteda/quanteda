#include <Rcpp.h>
#include <string>
#include <algorithm>

using namespace Rcpp;
using namespace std;

std::string join(std::vector< std::string > ngram, 
                 std::string delim){
  if(ngram.size() == 0) return "";
  std::string token_ngram = ngram[0];
  int len_ngram = ngram.size();
  for (int i = 1; i < len_ngram; i++) {
    token_ngram = token_ngram + delim + ngram[i];
  }
  return token_ngram;
}


std::vector< std::vector<std::string> > branch(std::vector< std::string > tokens,
                                  int start,
                                  int n, 
                                  std::vector< int > ks,
                                  std::vector<std::string> ngram
){

  int len_tokens = tokens.size();
  int len_ks = ks.size();
  std::vector< std::vector<std::string> > ngrams_done;
  
  ngram.push_back(tokens[start]);
  if(ngram.size() < n){
    for (int j = 0; j < len_ks; j++){
      int next = start + ks[j];
      if(next > len_tokens - 1) break;
      std::vector< std::vector<std::string> > ngrams_done_temp = branch(tokens, next, n, ks, ngram);
      ngrams_done_temp.reserve(ngrams_done.size() + ngrams_done_temp.size() ); 
      ngrams_done.insert(ngrams_done.end(), ngrams_done_temp.begin(), ngrams_done_temp.end() );
    }
  }else{
    ngrams_done.push_back(ngram);
  }
  return ngrams_done;
}




// [[Rcpp::export]]
std::vector< std::string > skipgramcpp(std::vector< std::string > tokens,
                                    std::vector< int > ns, 
                                    std::vector< int > ks, 
                                    std::string delim 
){
  std::vector<std::string> ngram;
  std::vector< std::vector<std::string> > ngrams;
  std::vector< std::string > tokens_ngram;
  int len_ns = ns.size();
  int len_tokens = tokens.size();
  for (int g = 0; g < len_ns; g++) {
    int n = ns[g];
    for (int h = 0; h < len_tokens; h++) {
      ngrams = branch(tokens, h, n, ks, ngram);
      int len_ngrams = ngrams.size();
      for (int h = 0; h < len_ngrams; h++) {
        tokens_ngram.push_back(join(ngrams[h], delim));
      }
    }
  }
  return tokens_ngram;
}



// [[Rcpp::export]]
std::vector< std::vector<std::string> > skipgramcppl(SEXP x,
                                                     std::vector< int > ns, 
                                                     std::vector< int > ks,
                                                     std::string delim){
  
  //Rcpp::CharacterVector texts(list);                                     
  Rcpp::List texts(x);
  std::vector< std::vector<std::string> > texts_sg;
  
  int len_texts = texts.size();
  for (int g=0; g < len_texts; g++){
    //Rcout << "Text" << g << "\n";
    std::vector <std::string> words = texts[g];
    texts_sg.push_back(skipgramcpp(words, ns, ks, delim));
  }
  return texts_sg;
}

