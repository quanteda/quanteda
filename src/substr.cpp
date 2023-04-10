#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string cpp_substr(std::string text, int len = 0) {
    int n = 0;
    size_t i = 0;
    while (i < text.length()) {
        int cplen = 0;
        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;   
        } else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;   
        } else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        } else {
            cplen = 1;
        }
        if (cplen > 0) {
            n++;
            //Rcout << i << " " << n << " " << cplen << ": "<< text.substr(i, cplen) << "\n";
        }
        if (n > len)
            return text.substr(0, i);
        i += cplen;
    }
    return text;
}


/*** R
cpp_substr("今天周五123", 3)
cpp_substr("今天周五123", 7)

require(stringi)
txt <- stri_rand_strings(1, 100, "[一-龠]")
microbenchmark::microbenchmark(
    cpp_substr(txt, 50),
    stri_sub(txt, 1, 50),
    stri_sub(txt, -50, 100)
)
*/
