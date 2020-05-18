#ifndef QUANTEDA // prevent redefining
#define QUANTEDA

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include <unordered_map>
#include <unordered_set>
#include <limits>
#include <algorithm>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
// [[Rcpp::depends(RcppParallel)]]
using namespace RcppParallel;

#define CLANG_VERSION (__clang_major__ * 10000 + __clang_minor__ * 100 + __clang_patchlevel__)

// global setting for unordered_map and unordered_set 
extern float GLOBAL_PATTERN_MAX_LOAD_FACTOR;
extern float GLOBAL_NGRAMS_MAX_LOAD_FACTOR;

// compiler has to be newer than clang 3.30 or gcc 4.8.1
#if RCPP_PARALLEL_USE_TBB && (CLANG_VERSION >= 30300 || GCC_VERSION >= 40801) 
#define QUANTEDA_USE_TBB true // tbb.h is loaded automatically by RcppParallel.h
#else
#define QUANTEDA_USE_TBB false
#endif

namespace quanteda{
    
    typedef ListOf<IntegerVector> Tokens;
    typedef std::vector<unsigned int> Text;
    typedef std::vector<Text> Texts;
    
#if QUANTEDA_USE_TBB
    typedef tbb::atomic<int> IntParam;
    typedef tbb::atomic<unsigned int> UintParam;
    typedef tbb::atomic<long> LongParam;
    typedef tbb::atomic<double> DoubleParam;
    typedef tbb::concurrent_vector<int> IntParams;
    typedef tbb::concurrent_vector<long> LongParams;
    typedef tbb::concurrent_vector<double> DoubleParams;
    typedef tbb::concurrent_vector<std::string> StringParams;
    typedef tbb::spin_mutex Mutex;
#else
    typedef int IntParam;
    typedef unsigned int UintParam;
    typedef long LongParam;
    typedef double DoubleParam;
    typedef std::vector<int> IntParams;
    typedef std::vector<long> LongParams;
    typedef std::vector<double> DoubleParams;
    typedef std::vector<std::string> StringParams;
#endif    
    
    // Ngram functions and objects -------------------------------------------------------
    
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector<Ngram> Ngrams;
    typedef std::string Type;
    typedef std::vector<Type> Types;
    
    struct hash_ngram {
        std::size_t operator() (const Ngram &vec) const {
            unsigned int seed = 0;
            for (std::size_t i = 0; i < vec.size(); i++) {
                seed += vec[i] * (256 ^ i);
            }
            return std::hash<unsigned int>()(seed);
        }
    };
    
    struct equal_ngram {
        bool operator() (const Ngram &vec1, const Ngram &vec2) const { 
            return (vec1 == vec2);
        }
    };
    
#if QUANTEDA_USE_TBB
    typedef tbb::atomic<unsigned int> IdNgram;
    typedef tbb::concurrent_unordered_multimap<Ngram, UintParam, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef tbb::concurrent_unordered_map<Ngram, UintParam, hash_ngram, equal_ngram> MapNgrams;
    typedef tbb::concurrent_unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef tbb::concurrent_vector<Ngram> VecNgrams;
    typedef tbb::concurrent_unordered_set<unsigned int> SetUnigrams;
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef tbb::concurrent_vector<Triplet> Triplets; // for fcm_mt, ca_mt, wordfish_mt
#else
    typedef unsigned int IdNgram;
    typedef std::unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef std::unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef std::unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef std::vector<Ngram> VecNgrams;
    typedef std::unordered_set<unsigned int> SetUnigrams;
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef std::vector<Triplet> Triplets; // for fcm_mt, ca_mt, wordfish_mt
#endif    
    
    
    inline String join_strings(CharacterVector &tokens_, 
                       const String delim_ = " "){
        
        if (tokens_.size() == 0) return "";
        String token_ = tokens_[0];
        for (unsigned int i = 1; i < (unsigned int)tokens_.size(); i++) {
          token_ += delim_;
          token_ += tokens_[i];
        }
        token_.set_encoding(CE_UTF8);
        return token_;
    }
    
    inline std::string join_strings(std::vector<std::string> &tokens, 
                            const std::string delim = " "){
        if (tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (std::size_t i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }
    
    inline String join_strings(std::vector<unsigned int> &tokens, 
                       CharacterVector types_, 
                       const String delim_ = " ") {
        
        String token_("");
        if (tokens.size() > 0) {
            if (tokens[0] != 0) {
                token_ += types_[tokens[0] - 1];
            }
            for (std::size_t j = 1; j < tokens.size(); j++) {
                if (tokens[j] != 0) {
                    token_ += delim_;
                    token_ += types_[tokens[j] - 1];
                }
            }
            token_.set_encoding(CE_UTF8);
        }
        return token_;
    }
    
    inline bool has_na(IntegerVector vec_) {
        for (unsigned int i = 0; i < (unsigned int)vec_.size(); ++i) {
            if (vec_[i] == NA_INTEGER) return true;
        }
        return false;
    }
    
    /* 
     * This function is faster than Rcpp::wrap() but the stability need to be evaluated.
     */
    inline List as_list(Texts &texts, bool sort = false){
        List list(texts.size());
        for (std::size_t h = 0; h < texts.size(); h++) {
            Text text = texts[h];
            IntegerVector temp = Rcpp::wrap(text);
            list[h] = temp;
        }
        return list;
    }
    
    inline S4 to_matrix(Triplets& tri, int nrow, int ncol, bool symmetric) {
        
        std::size_t l = tri.size();
        IntegerVector dim_ = IntegerVector::create(nrow, ncol);
        IntegerVector i_(l), j_(l);
        NumericVector x_(l);
        
        for (std::size_t k = 0; k < tri.size(); k++) {
            i_[k] = std::get<0>(tri[k]);
            j_[k] = std::get<1>(tri[k]);
            x_[k] = std::get<2>(tri[k]);
        }
        if (symmetric) {
            S4 simil_("dsTMatrix");
            simil_.slot("i") = i_;
            simil_.slot("j") = j_;
            simil_.slot("x") = x_;
            simil_.slot("Dim") = dim_;
            simil_.slot("uplo") = "U";
            return simil_;
        } else {
            S4 simil_("dgTMatrix");
            simil_.slot("i") = i_;
            simil_.slot("j") = j_;
            simil_.slot("x") = x_;
            simil_.slot("Dim") = dim_;
            return simil_;
        }
    }

    inline std::vector<std::size_t> register_ngrams(List patterns_, SetNgrams &set) {
        
        set.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
        Ngrams patterns = Rcpp::as<Ngrams>(patterns_);
        std::vector<std::size_t> spans(patterns.size());
        for (size_t g = 0; g < patterns.size(); g++) {
            set.insert(patterns[g]);
            spans[g] = patterns[g].size();
        }
        
        // Rcout << "current max_load_factor: " << set.max_load_factor() << std::endl;
        // Rcout << "current size           : " << set.size() << std::endl;
        // Rcout << "current bucket_count   : " << set.unsafe_bucket_count() << std::endl;
        // Rcout << "current load_factor    : " << set.load_factor() << std::endl;
        
        sort(spans.begin(), spans.end());
        spans.erase(unique(spans.begin(), spans.end()), spans.end());
        std::reverse(std::begin(spans), std::end(spans));
        return spans;
    }

    inline std::vector<std::size_t> register_ngrams(List patterns_, IntegerVector ids_, MapNgrams &map) {

        map.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
        Ngrams patterns = Rcpp::as<Ngrams>(patterns_);
        std::vector<unsigned int> ids = Rcpp::as< std::vector<unsigned int> >(ids_);
        std::vector<std::size_t> spans(patterns.size());
        for (size_t g = 0; g < std::min(patterns.size(), ids.size()); g++) {
            map.insert(std::pair<Ngram, IdNgram>(patterns[g], ids[g]));
            spans[g] = patterns[g].size();
        }

        // Rcout << "current max_load_factor: " << map.max_load_factor() << std::endl;
        // Rcout << "current size           : " << map.size() << std::endl;
        // Rcout << "current bucket_count   : " << map.unsafe_bucket_count() << std::endl;
        // Rcout << "current load_factor    : " << map.load_factor() << std::endl;

        sort(spans.begin(), spans.end());
        spans.erase(unique(spans.begin(), spans.end()), spans.end());
        std::reverse(std::begin(spans), std::end(spans));
        return spans;
    }
    
}

#endif
