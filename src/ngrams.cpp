#include <Rcpp.h>
#include <string>
//#include <map>
#include <algorithm>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector< std::string > ngramcpp(std::vector< std::string > words,
                                    std::vector< int > ns, 
                                    int k,
                                    std::string delim 
){
  std::vector <std::string> ngams;
  int len_ns = ns.size();
  int len_words = words.size();
  for (int g = 0; g < len_ns; g++) {
    int n = ns[g];
    //Rcout << n << "\n";
    if(n > 1){
      for (int h = 0; h < len_words; h++) {
        std::string ngram = words[h];
        int count = 1;
        for (int i = std::max(h, h - (k * n)); i < std::min(len_words, h + (k * n) + 1); i += k ) {  
          //Rcout << h << "-" << i << delim << words[h] << "-" << words[i] << "\n";
          if (h == i) continue;
          
          ngram = ngram + delim + words[i];
          count++;
          if (count == n) {
            ngams.push_back(ngram);
            break;
          }
        }
      }
    }else{
      for (int h = 0; h < len_words; h++) {
        ngams.push_back(words[h]);
      }
    }
  }
  return ngams;
}

// [[Rcpp::export]]
std::vector< std::string > skipgramcpp(std::vector< std::string > words,
                                       std::vector< int > ns, 
                                       std::vector< int > ks, 
                                       std::string delim 
){
  std::vector< std::string > skipgrams;
  int len_ks = ks.size();
  for (int j = 0; j < len_ks; j++) {
    //Rcout << "Skip" << k << "\n";
    int k = ks[j];
    std::vector <std::string> skipgrams_new = ngramcpp(words, ns, k, delim);
    skipgrams.insert(skipgrams.end(), skipgrams_new.begin(), skipgrams_new.end() );
  }
  return skipgrams;
}

// [[Rcpp::export]]
std::vector< std::vector<std::string> > ngramcppl(SEXP x,
                                                  std::vector< int > ns,
                                                  int k, 
                                                  std::string delim){
  
  //Rcpp::CharacterVector texts(list);                                     
  Rcpp::List texts(x);
  std::vector< std::vector<std::string> > texts_ng;
  
  int len_texts = texts.size();
  for (int g=0; g < len_texts; g++){
    //Rcout << "Text" << g << "\n";
    std::vector< std::string > words = texts[g];
    texts_ng.push_back(ngramcpp(words, ns, k, delim));
  }
  return texts_ng;
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

