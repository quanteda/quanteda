#include "tbb/concurrent_unordered_map.h"
#include "tbb/concurrent_unordered_set.h"
#include "tbb/concurrent_vector.h"
using namespace Rcpp;
using namespace std;
using namespace tbb;

#ifndef __QUANTEDA__
#define __QUANTEDA__


#define RCPP_USING_CXX11
namespace quanteda{

    inline String join(CharacterVector &tokens, String &delim){
        if (tokens.size() == 0) return "";
        String token = tokens[0];
        for (size_t i = 1; i < tokens.size(); i++) {
          token += delim;
          token += tokens[i];
          //Rcout << "Joined " << token.get_cstring()  << "\n";
        }
        token.set_encoding(CE_UTF8);
        return token;
      }
    
    inline std::string join(std::vector< std::string > &tokens, std::string &delim){
        if (tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (size_t i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }

    inline bool has_na(IntegerVector vec) {
        for (size_t i = 0; i < vec.size(); ++i) {
            if (vec[i] == NA_INTEGER) return true;
        }
       return false;
    }

    inline List as_list(std::vector< std::vector<unsigned int> > &tokens){
        List list(tokens.size());
        for (size_t h = 0; h < tokens.size(); h++) {
            if (tokens[h].size() > 0) {
                IntegerVector temp = Rcpp::wrap(tokens[h]);
                list[h] = temp;
            } else {
                list[h] = R_NilValue;
            }
            
        }
        return list;
    }
    
}

namespace ngrams {
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector< std::vector<unsigned int> > Ngrams;
    typedef std::vector<unsigned int> Text;
    typedef std::vector< std::vector<unsigned int> > Texts;
    struct hash_ngram {
        std::size_t operator() (const Ngram &vec) const {
            //unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
            //return std::hash<unsigned int>()(seed);
            unsigned int seed = 0;
            for (size_t i = 0; i < vec.size(); i++) {
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
    typedef concurrent_unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef concurrent_unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
}

#endif
