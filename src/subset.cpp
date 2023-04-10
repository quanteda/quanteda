#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void cpp_sub(std::string text) {
    for (size_t i = 0; i < text.length(); i++) {
        int cplen = 1;
        if ((text[i] & 0xf8) == 0xf0) cplen = 4;
        else if((text[i] & 0xf0) == 0xe0) cplen = 3;
        else if((text[i] & 0xe0) == 0xc0) cplen = 2;
        if ((i + cplen) > text.length()) cplen = 1;
        
        Rcout << text.substr(i, cplen) << "\n";
        i += cplen;
    }
}


/*** R
cpp_sub("今天周五123")
*/
