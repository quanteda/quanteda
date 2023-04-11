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

typedef tbb::concurrent_unordered_map<Type, int> MultiMapIndex;
typedef std::string Pattern;
typedef std::vector<Pattern> Patterns;

void index_glob(Types &types, MultiMapIndex &index,
                std::string wildcard, int side, int len) {
    
    std::size_t H = types.size();
    Types temp(H);
    
    for (size_t h = 0; h < H; h++) {
        if (side == 1) {
            std::string value = substr_left(types[h], len) + wildcard;
        } else if (side == 2) {
            std::string value = wildcard + substr_right(types[h], len);
        }
        map_keys.insert(std::pair<Type, int>(value, h));
    }
}


// [[Rcpp::export]]
CharacterVector cpp_index_types(const CharacterVector &patterns_, 
                                const CharacterVector &types_) {
    
    dev::Timer timer;
    dev::start_timer("Convert", timer);
    
    Patterns patterns = Rcpp::as<Patterns>(patterns_);
    Types types = Rcpp::as<Types>(types_);
    
    std::vector<int> len;
    len.reserve(patterns.size());
    for (size_t i = 0; i < patterns.size(); i++) {
        std::string pattern = patterns[i];
        if (pattern[0] == "*") != (pattern[patten.length()] == "*") {
            len.push_back(patten.length() - 1);
        }
    }

    dev::stop_timer("Convert", timer);
    
    dev::start_timer("Indexing", timer);
    
    MultiMapIndex index;
    
    std::size_t H = len.size();
    Texts temp(H);
// #if QUANTEDA_USE_TBB
//     tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
//     
//     });
// #else
    for (size_t h = 0; h < H; h++) {
        index_glob(types, index, "*", 1, len[h]);
        index_glob(types, index, "*", 2, len[h]);
    }
    index_glob(types, index, "?", 1, -1);
    index_glob(types, index, "?", 2, -1);
// #endif
    
    dev::start_timer("List", timer);
    List result_(patterns.size());
    for (size_t i = 0; i < patterns.size(); i++) {
        std::string pattern = patterns[i];
        auto range = index.equal_range(pattern);
        Rcout << "size:" <<   range.secod - range.first << "\n"
        for (auto it = range.first; it != range.second; ++it) {
            int pos = it->second;
        }
    }
    dev::stop_timer("List", timer);
    
    return result_;
}

/*** R
cpp_substr("今天周五123", 3, 1)
cpp_substr("今天周五123", 5, 2)
*/
