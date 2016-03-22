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


void skip(std::vector< std::string > &tokens,
                                  unsigned int start,
                                  unsigned int n, 
                                  std::vector< int > ks,
                                  std::vector<std::string> ngram,
                                  std::vector< std::vector<std::string> > &ngrams
){

  int len_tokens = tokens.size();
  int len_ks = ks.size();
   
  ngram.push_back(tokens[start]);
  if(ngram.size() < n){
    for (int j = 0; j < len_ks; j++){
      int next = start + ks[j];
      if(next > len_tokens - 1) break;
      skip(tokens, next, n, ks, ngram, ngrams);
    }
  }else{
    ngrams.push_back(ngram);
  }
}

// [[Rcpp::export]]
std::vector< std::string > skipgramcpp(std::vector< std::string > tokens,
                                    std::vector< int > ns, 
                                    std::vector< int > ks, 
                                    std::string delim 
){
  
  
  // Generate skipgrams recursively
  std::vector<std::string> ngram;
  std::vector< std::vector<std::string> > ngrams; // For the rerusive function
  int len_ns = ns.size();
  int len_tokens = tokens.size();
  
  for (int g = 0; g < len_ns; g++) {
    int n = ns[g];
    for (int h = 0; h < len_tokens; h++) {
      skip(tokens, h, n, ks, ngram, ngrams); // Get ngrams as reference
    }
  }
  
  // Join elemtns of ngrams
  std::vector< std::string > tokens_ngram;
  int len_ngrams = ngrams.size();
  for (int h = 0; h < len_ngrams; h++) {
    tokens_ngram.push_back(join(ngrams[h], delim));
  }
  return tokens_ngram;
}



// [[Rcpp::export]]
std::vector< std::vector<std::string> > skipgramcppl(SEXP units,
                                                     std::vector< int > ns, 
                                                     std::vector< int > ks,
                                                     std::string delim){
  
  //Rcpp::CharacterVector units(list);                                     
  Rcpp::List units_in(units);
  std::vector< std::vector<std::string> > units_out;
  
  int len_units = units_in.size();
  for (int g=0; g < len_units; g++){
    //Rcout << "Unit" << g << "\n";
    std::vector <std::string> tokens = units_in[g];
    units_out.push_back(skipgramcpp(tokens, ns, ks, delim));
  }
  return units_out;
}

