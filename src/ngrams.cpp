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
StringVector skipgramcpp(std::vector < std::string > tokens,
                         std::vector < int > ns, 
                         std::vector < int > ks, 
                         std::string delim) {
    
    // Generate skipgrams recursively
    std::vector<std::string> ngram;
    std::vector< std::vector<std::string> > ngrams; // For the recusive function
    int len_ns = ns.size();
    int len_tokens = tokens.size();
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        for (int h = 0; h < len_tokens; h++) {
            skip(tokens, h, n, ks, ngram, ngrams); // Get ngrams as reference
        }
    }
    
    // Join elements of ngrams
    StringVector tokens_ngram;
    int len_ngrams = ngrams.size();
    for (int h = 0; h < len_ngrams; h++) {
        Rcpp::String str(join(ngrams[h], delim));
        str.set_encoding(CE_UTF8);
        tokens_ngram.push_back(str);
    }
    return tokens_ngram;
}



