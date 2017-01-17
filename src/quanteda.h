using namespace Rcpp;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

#ifndef __QUANTEDA__
#define __QUANTEDA__


#define RCPP_USING_CXX11
namespace quanteda{
    
    typedef std::vector<unsigned int> Text;
    typedef std::vector<Text> Texts;
#if RCPP_PARALLEL_USE_TBB
    typedef tbb::atomic<int> IntParam;
    typedef tbb::atomic<long> LongParam;
    typedef tbb::atomic<double> DoubleParam;
    typedef tbb::concurrent_vector<int> IntParams;
    typedef tbb::concurrent_vector<long> LongParams;
    typedef tbb::concurrent_vector<double> DoubleParams;
#else
    typedef int IntParam;
    typedef long LongParam;
    typedef double DoubleParam;
    typedef std::vector<int> IntParams;
    typedef std::vector<long> LongParams;
    typedef std::vector<double> DoubleParams;
#endif    
    
    inline String join(CharacterVector &tokens, String &delim){
        if (tokens.size() == 0) return "";
        String token = tokens[0];
        for (unsigned int i = 1; i < tokens.size(); i++) {
          token += delim;
          token += tokens[i];
        }
        token.set_encoding(CE_UTF8);
        return token;
    }
    
    inline std::string join(std::vector< std::string > &tokens, std::string &delim){
        if (tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (unsigned int i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }

    inline bool has_na(IntegerVector vec) {
        for (unsigned int i = 0; i < vec.size(); ++i) {
            if (vec[i] == NA_INTEGER) return true;
        }
       return false;
    }
}

namespace ngrams {
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector<Ngram> Ngrams;
    typedef std::string Type;
    typedef std::vector<Type> Types;
    
    struct hash_ngram {
            std::size_t operator() (const Ngram &vec) const {
            unsigned int seed = 0;
            for (std::size_t i = 0; i < vec.size(); i++) {
                seed += vec[i] << (i * 8); // shift elements 8 bit
            }
            return std::hash<unsigned int>()(seed);
        }
    };
    
    struct equal_ngram {
        bool operator() (const Ngram &vec1, const Ngram &vec2) const { 
            return (vec1 == vec2);
        }
    };

#if RCPP_PARALLEL_USE_TBB
    // TBB header is loaded automatically by the macro
    typedef tbb::atomic<int> IdNgram; // tbb::atomic does not suppport unsigined int
    typedef tbb::concurrent_unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef tbb::concurrent_unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef tbb::concurrent_vector<Ngram> VecNgrams;
    typedef tbb::concurrent_unordered_set<unsigned int> SetUnigrams;
#else
    typedef int IdNgram;
    typedef std::unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef std::unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef std::unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef std::vector<Ngram> VecNgrams;
    typedef std::unordered_set<unsigned int> SetUnigrams;
#endif    

}

#endif
