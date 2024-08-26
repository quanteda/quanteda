#ifndef QUANTEDA4 // prevent redefining
#define QUANTEDA4

#ifndef ARMA_64BIT_WORD
#define ARMA_64BIT_WORD
#endif

//#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <atomic>
#include <unordered_map>
#include <unordered_set>
#include <limits>
#include <algorithm>
#include "tokens.h"
#ifdef TBB
#include <tbb/tbb.h>
#ifdef ONETBB_SPEC_VERSION
using namespace oneapi; // only Windows R 4.3.x or later
#endif
#define QUANTEDA_USE_TBB true
#else
#define QUANTEDA_USE_TBB false
#endif

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

// NOTE: used in textstats
const float GLOBAL_PATTERN_MAX_LOAD_FACTOR = 0.05;
const float GLOBAL_NGRAMS_MAX_LOAD_FACTOR = 0.25;

namespace quanteda{
    
    typedef ListOf<IntegerVector> Tokens;
    typedef XPtr<TokensObj> TokensPtr;

    // typedef ListOf<IntegerVector> Tokens;
    // typedef std::vector<unsigned int> Text;
    // typedef std::vector<Text> Texts;
    
    typedef std::atomic<int> IntParam;
    typedef std::atomic<unsigned int> UintParam;
    typedef std::atomic<long> LongParam;
    typedef std::atomic<double> DoubleParam;
    
#if QUANTEDA_USE_TBB
    typedef tbb::concurrent_vector<int> IntParams;
    typedef tbb::concurrent_vector<long> LongParams;
    typedef tbb::concurrent_vector<double> DoubleParams;
    typedef tbb::concurrent_vector<std::string> StringParams;
#else
    typedef std::vector<int> IntParams;
    typedef std::vector<long> LongParams;
    typedef std::vector<double> DoubleParams;
    typedef std::vector<std::string> StringParams;
#endif    
    
    // Ngram functions and objects -------------------------------------------------------
    
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector<Ngram> Ngrams;
    
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
    
    typedef std::atomic<unsigned int> IdNgram;
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
#if QUANTEDA_USE_TBB
    typedef tbb::concurrent_unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef tbb::concurrent_unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef tbb::concurrent_vector<Ngram> VecNgrams;
    typedef tbb::concurrent_unordered_set<unsigned int> SetUnigrams;
    typedef tbb::concurrent_vector<Triplet> Triplets; // for fcm_mt, ca_mt, wordfish_mt
#else
    typedef std::unordered_multimap<Ngram, unsigned int, hash_ngram, equal_ngram> MultiMapNgrams;
    typedef std::unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> MapNgrams;
    typedef std::unordered_set<Ngram, hash_ngram, equal_ngram> SetNgrams;
    typedef std::vector<Ngram> VecNgrams;
    typedef std::unordered_set<unsigned int> SetUnigrams;
    typedef std::vector<Triplet> Triplets; // for fcm_mt, ca_mt, wordfish_mt
#endif    
    
        
    inline String join_strings(CharacterVector &tokens_, 
                               const String &delim_ = " "){
        
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
                                    const std::string &delim = " "){
        if (tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (std::size_t i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }
    
    // NOTE: used in textstats
    inline String join_strings(std::vector<unsigned int> &tokens,
                               const CharacterVector &types_,
                               const String &delim_ = " ") {

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
    
    inline std::string join_strings(std::vector<unsigned int> &tokens,
                                    const Types &types,
                                    const std::string &delim = " ") {

        std::string token("");
        if (tokens.size() > 0) {
            if (tokens[0] != 0) {
                token += types[tokens[0] - 1];
            }
            for (std::size_t j = 1; j < tokens.size(); j++) {
                if (tokens[j] != 0) {
                    token += delim;
                    token += types[tokens[j] - 1];
                }
            }
        }
        return token;
    }
    
    inline bool is_encoded(String &delim_){
        if (delim_.get_encoding() > 0) {
            return true;
        }
        return false;
    }
    
    inline bool is_encoded(CharacterVector &types_){
        for (unsigned int i = 0; i < (unsigned int)types_.size(); i++) {
            String type_ = types_[i];
            if (type_.get_encoding() > 0) {
                return true;
            }
        }
        return false;
    }
    
    inline CharacterVector encode(Types &types){
        CharacterVector types_(types.size());
        for (std::size_t i = 0; i < types.size(); i++) {
            String type_ = types[i];
            type_.set_encoding(CE_UTF8);
            types_[i] = type_;
        }
        return(types_);
    }
    
    inline bool has_na(IntegerVector vec_) {
        for (unsigned int i = 0; i < (unsigned int)vec_.size(); ++i) {
            if (vec_[i] == NA_INTEGER) return true;
        }
        return false;
    }
    
    inline List as_list(Texts &texts){
      List texts_(texts.size());
      for (std::size_t h = 0; h < texts.size(); h++) {
        Text text = texts[h];
        IntegerVector text_ = wrap(text);
        texts_[h] = text_;
      }
      return texts_;
    }
    
    inline S4 to_matrix(Triplets& tri, int nrow, int ncol, bool symmetric) {
        
        std::size_t l = tri.size();
        IntegerVector dim_ = IntegerVector::create(nrow, ncol);
        List dimnames_ = List::create(R_NilValue, R_NilValue);
        IntegerVector i_(l), j_(l);
        NumericVector x_(l);
        
        std::size_t k = 0;
        for (Triplet t : tri) {
            i_[k] = std::get<0>(t);
            j_[k] = std::get<1>(t);
            x_[k] = std::get<2>(t);
            k++;
        }
        if (symmetric) {
            S4 simil_("dsTMatrix");
            simil_.slot("i") = i_;
            simil_.slot("j") = j_;
            simil_.slot("x") = x_;
            simil_.slot("Dim") = dim_;
            simil_.slot("Dimnames") = dimnames_;
            simil_.slot("uplo") = "U";
            return simil_;
        } else {
            S4 simil_("dgTMatrix");
            simil_.slot("i") = i_;
            simil_.slot("j") = j_;
            simil_.slot("x") = x_;
            simil_.slot("Dim") = dim_;
            simil_.slot("Dimnames") = dimnames_;
            return simil_;
        }
    }
    
    inline Ngrams to_ngrams(List patterns, bool no_padding = false) {
        Ngrams ngrams;
        std::size_t I = patterns.size();
        ngrams.reserve(I);
        for (std::size_t i = 0; i < I; i++) {
            if (!Rcpp::is<IntegerVector>(patterns[i]))
                throw std::invalid_argument("Invalid patterns");
            IntegerVector pattern = patterns[i];
            std::size_t J = pattern.size();
            Ngram ngram(J);
            for (std::size_t j = 0; j < J; j++) {
                if (no_padding && pattern[j] == 0)
                    break;
                if (pattern[j] < 0 || IntegerVector::is_na(pattern[j]))
                    break;
                ngram[j] = pattern[j];
            }
            ngrams.push_back(ngram);
        }
        return(ngrams);
    }
    
    inline std::vector<std::size_t> register_ngrams(List patterns_, 
                                                    SetNgrams &set,
                                                    bool no_padding = false) {
        
        set.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
        Ngrams patterns = to_ngrams(patterns_, no_padding);
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

    inline std::vector<std::size_t> register_ngrams(List patterns_, 
                                                    IntegerVector ids_, 
                                                    MapNgrams &map,
                                                    bool no_padding = false) {

        map.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
        Ngrams patterns = to_ngrams(patterns_, no_padding);
        std::vector<unsigned int> ids = Rcpp::as< std::vector<unsigned int> >(ids_);
        std::vector<std::size_t> spans(patterns.size());
        for (size_t g = 0; g < std::min(patterns.size(), ids.size()); g++) {
            map.insert(std::pair<Ngram, unsigned int>(patterns[g], ids[g]));
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
