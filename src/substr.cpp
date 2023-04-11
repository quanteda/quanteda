#include "lib.h"
#include "dev.h"
using namespace quanteda;

std::string substr_left(std::string &text, int len = 0) {
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
        } else if ((text[i] & 0x80) == 0) {
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

std::string substr_right(std::string &text, int len = 0) {
    int n = 0;
    size_t i = text.length();
    while (0 <= i) {
        int cplen = 0;
        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;   
        } else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;   
        } else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        } else if ((text[i] & 0x80) == 0) {
            cplen = 1;
        }
        if (cplen > 0) {
            n++;
            //Rcout << i << " " << n << " " << cplen << ": "<< text.substr(i, cplen) << "\n";
        }
        if (n > len)
            return text.substr(i, text.length());
        i -= 1;
    }
    return text;
}

// [[Rcpp::export]]
CharacterVector cpp_substr(const CharacterVector texts_, 
                           int len = 0, int side = 1) {
    
    dev::Timer timer;
    dev::start_timer("Convert", timer);
    
    int G = texts_.size();
    Types texts(G);
    for (int g = 0; g < texts_.size(); g++) {
        texts[g] = Rcpp::as<Type>(texts_[g]);
    }
    //Types texts = Rcpp::as<Types>(texts_);
    std::size_t H = texts.size();
    Types temp(H);
    dev::stop_timer("Convert", timer);
    
    dev::start_timer("Substring", timer);
#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            if (side == 1) {
                temp[h] = substr_left(texts[h], len);
            } else if (side == 2) {
                temp[h] = substr_right(texts[h], len);
            } else {
                temp[h] = texts[h];    
            }
        }    
    });
#else
    for (size_t h = 0; h < H; h++) {
        if (side == 1) {
            temp[h] = substr_left(texts[h], len)
        } else if (side == 2) {
            temp[h] = substr_right(texts[h], len)
        } else {
            temp[h] = texts[h];    
        }
    }
#endif
    dev::stop_timer("Substring", timer);
    
    dev::start_timer("Encode", timer);
    CharacterVector result = encode(temp);
    dev::stop_timer("Encode", timer);
    return result;
}

/*** R
cpp_substr("今天周五123", 3, 1)
cpp_substr("今天周五123", 5, 2)
*/
