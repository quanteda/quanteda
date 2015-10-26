#include <Rcpp.h>
#include <string>
//#include <map>
#include <algorithm>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector< std::string > ngramcpp(std::vector< std::string > words,
                                    int n, int skip, std::string delim 
){
  
  std::vector <std::string> ngams;
  int len = words.size();
  for( int h = 0; h < len; h++ ) {
    int count = 1;
    std::string ngram = words[h];
    //int i_start = std::max(h, h - (skip * n));
    //int i_last = std::min(len, h +(skip * n)) + 1;
    //Rcout << i_start << "to" << i_last << "\n";
    //for( int i = i_start; i < i_last; i += skip ) {
    for( int i = std::max(h, h - (skip * n)); i < std::min(len, h + (skip * n) + 1); i += skip ) {  
      if( h == i) continue;
      //Rcout << h << "-" << i << delim << words[h] << "-" << words[i] << "\n";
      ngram = ngram + delim + words[i];
      count++;
      if(count == n){
        ngams.push_back(ngram);
        break;
      }
    }
  }
  return ngams;
}

// [[Rcpp::export]]
std::vector< std::string > skipgramcpp(std::vector< std::string > words,
                                       int n, int skip, std::string delim 
){
  std::vector< std::string > skipgrams;
  for(int k = 1; k <= skip; k++){
    //Rcout << "Skip" << k << "\n";
    std::vector <std::string> skipgrams_new = ngramcpp(words, n, k, delim);
    skipgrams.insert(skipgrams.end(), skipgrams_new.begin(), skipgrams_new.end() );
  }
  return skipgrams;
}

// [[Rcpp::export]]
std::vector< std::vector<std::string> > ngramcppl(SEXP x, int n, int skip, std::string delim){
  
  //Rcpp::CharacterVector texts(list);                                     
  Rcpp::List texts(x);
  std::vector< std::vector<std::string> > texts_ng;
  
  int len_texts = texts.size();
  for (int g=0; g < len_texts; g++){
    Rcout << "Text" << g << "\n";
    std::vector< std::string > words = texts[g];
    texts_ng.push_back(ngramcpp(words, n, skip, delim));
  }
  return texts_ng;
}

// [[Rcpp::export]]
std::vector< std::vector<std::string> > skipgramcppl(SEXP x, int n, int skip, std::string delim){
  
  //Rcpp::CharacterVector texts(list);                                     
  Rcpp::List texts(x);
  std::vector< std::vector<std::string> > texts_sg;
  
  int len_texts = texts.size();
  for (int g=0; g < len_texts; g++){
    //Rcout << "Text" << g << "\n";
    std::vector <std::string> words = texts[g];
    texts_sg.push_back(skipgramcpp(words, n, skip, delim));
  }
  return texts_sg;
}


