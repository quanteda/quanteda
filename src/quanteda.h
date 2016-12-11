#include "tbb/concurrent_unordered_map.h"
#include "tbb/concurrent_unordered_set.h"
using namespace Rcpp;
using namespace std;
using namespace tbb;

#ifndef __QUANTEDA__
#define __QUANTEDA__


#define RCPP_USING_CXX11
namespace quanteda{

    inline String join(const CharacterVector &tokens, const String &delim){
        if(tokens.size() == 0) return "";
        String token = tokens[0];
        for (int i = 1; i < tokens.size(); i++) {
          token += delim;
          token += tokens[i];
          //Rcout << "Joined " << token.get_cstring()  << "\n";
        }
        token.set_encoding(CE_UTF8);
        return token;
      }
    
    inline std::string join(std::vector< std::string > tokens, std::string delim){
        if(tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (int i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }

    inline bool has_na(IntegerVector vec) {
        for (int i = 0; i < vec.size(); ++i) {
            if(vec[i] == NA_INTEGER) return true;
        }
       return false;
    }
    
}

namespace ngrams {
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector< std::vector<unsigned int> > Ngrams;
    //typedef std::vector<unsigned int> Text;
    typedef std::vector<int> Text;
    //typedef std::vector< std::vector<unsigned int> > Texts;
    typedef std::vector< std::vector<int> > Texts;
    struct hash_ngram {
        std::size_t operator() (const Ngram &vec) const {
            //unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
            //return std::hash<unsigned int>()(seed);
            unsigned int seed = 0;
            for(int i = 0; i < vec.size(); i++){
                seed += vec[i] << (i * 8); // shift elements 8 bit
            }
            return std::hash<unsigned int>()(seed);
        }
    };
    
    struct equal_ngram {
        bool operator() (const Ngram &vec1, const Ngram &vec2) const { 
            return (vec1 == vec2);
            //return true;
        }
    };
    typedef concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef concurrent_unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
}

#endif
