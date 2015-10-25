#include <Rcpp.h>
#include <string>
//#include <map>
#include <algorithm>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector<std::string> ngramcpp(std::vector< std::string > strings,
                                    int n, int window, std::string concatenator 
                                    ){
  
  // Rename parameters
  int step = window;
  std::string sep = concatenator;
  
  std::vector <std::string> ngams;
  int len = strings.size();
  for( int h = 0; h < len; h++ ) {
    int count = 1;
    std::string ngram = strings[h];
    //for( int i = std::max(0, h-window); i < std::min(len, h+window+1); i += step ) {
    for( int i = std::max(h, h - (step * n)); i < std::min(len, h + (step * n) + 1); i += step ) {  
      if( h == i) continue;
      // Rcout << h << "-" << i << ":" << strings[h] << "-" << strings[i] << "\n";
      ngram = ngram + sep + strings[i];
      count++;
      if (count == n) {
        ngams.push_back(ngram);
        break;
      }
    }
  }
  return ngams;
}
