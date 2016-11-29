#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
#include "dev.h"
#include "quanteda.h"
#include "tbb/concurrent_unordered_map.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace std;
using namespace quanteda;


namespace ngrams {
    typedef std::vector<unsigned int> Ngram;
    typedef std::vector<unsigned int> Ngrams;
    typedef std::vector<unsigned int> Text;
    typedef std::vector< std::vector<unsigned int> > Texts;

    struct hash_ngram {
        std::size_t operator() (const Ngram &vec) const {
            unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
            return std::hash<unsigned int>()(seed);
        }
    };
    
    struct equal_ngram {
        bool operator() (const Ngram &vec1, const Ngram &vec2) const { 
            return (vec1 == vec2);
        }
    };
}
using namespace ngrams;


int ngram_id(Ngram ngram,
             tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram){
    
    // Add new ID without multiple access
    unsigned int &id_ngram = map_ngram[ngram];

    if(id_ngram){
        //Rcout << "Old " << id_ngram << ": ";
        //dev::print_ngram_hashed(ngram);
        return id_ngram;
    }
    id_ngram = map_ngram.size();
    //Rcout << "New " << id_ngram << ": ";
    //dev::print_ngram_hashed(ngram);
    return id_ngram;
}

void skip(Text &tokens,
          unsigned int start,
          unsigned int n, 
          std::vector<int> skips,
          Ngram ngram,
          Ngrams &ngrams,
          tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram,
          int pos_tokens, int &pos_ngrams) {
    
    ngram[pos_tokens] = tokens[start];
    pos_tokens++;
    
    //Rcout << "Token " << tokens[start] << "\n";
    if(pos_tokens < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next < 0 || tokens.size() - 1 < next) break;
            if(tokens[next] == 0) break; // Skip padding
            //Rcout << "Join " << tokens[start] << " at " << pos_tokens << " " << next << "\n";
            skip(tokens, next, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams);
        }
    }else{
        
        ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
        
        //Rcout << "Add " << ngrams[pos_ngrams] << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
        pos_tokens = 0;
        pos_ngrams++;
    }
}


Ngrams skipgram(Text tokens,
                std::vector<int> ns, 
                std::vector<int> skips,
                tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram) {
    
    int pos_tokens = 0; // Position in tokens
    int pos_ngrams = 0; // Position in ngrams
    
    // Pre-allocate memory
    int size_reserve = 0;
    for (int k = 0; k < ns.size(); k++) {
        size_reserve += std::pow(skips.size(), ns[k]) * tokens.size();
    }
    Ngrams ngrams(size_reserve);
    
    // Generate skipgrams recursively
    for (int k = 0; k < ns.size(); k++) {
        int n = ns[k];
        Ngram ngram(n);
        for (int start = 0; start < tokens.size() - (n - 1); start++) {
            if(tokens[start] == 0) continue; // Skip padding
            skip(tokens, start, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams); // Get ngrams as reference
        }
    }
    ngrams.resize(pos_ngrams);
    return ngrams;
}

struct skipgram_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<int> ns;
    const std::vector<int> skips;
    tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram;
    
    // Constructor
    skipgram_mt(Texts &input_, Texts &output_, std::vector<int> ns_, std::vector<int> skips_, 
                tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram_):
                input(input_), output(output_), ns(ns_), skips(skips_), map_ngram(map_ngram_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            output[h] = skipgram(input[h], ns, skips, map_ngram);
        }
    }
};

/* 
 * This funciton constructs ngrams from tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used ngrams.tokens()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @n size of ngramss
 * @skips size of skip (this has to be 1 for ngrams)
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_ngram_mt_list(List texts_,
                            IntegerVector ns_,
                            IntegerVector skips_) {
    
    Texts input = Rcpp::as< Texts >(texts_);
    std::vector<int> ns = Rcpp::as< std::vector<int> >(ns_);
    std::vector<int> skips = Rcpp::as< std::vector<int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> map_ngram;

    Texts ouput(input.size());
    skipgram_mt skipgram_mt(input, ouput, ns, skips, map_ngram);
    
    // Apply skipgram_mt to blocked ranges
    parallelFor(0, input.size(), skipgram_mt);
    
    // Separate key and values of unordered_map
    List ids_unigram(map_ngram.size());
    for (std::pair<Ngram, unsigned int> iter : map_ngram){
        //Rcout << "ID " << to_string(iter.second) << ": ";
        //print_ngram_hashed(iter.first);
        ids_unigram[iter.second - 1] = iter.first;
    }
    List texts_ngram = Rcpp::wrap(ouput);
    
    return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                              Rcpp::Named("id_unigram") = ids_unigram);
}







/*** R

library(quanteda)
# txt <- c('a b c d e', 'c d e f g')
# toks <- tokens(txt, what='fastestword')
# res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
# res <- qatd_cpp_ngram_hashed_list(toks, 2, 1)
# res$text
# res$id_unigram

RcppParallel::setThreadOptions(numThreads = 2)
toks = rep(list(1:1000, 1001:2000), 10)
#toks = rep(list(1:10000, 10001:20000), 10000)
res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
#res <- qatd_cpp_ngram_hashed_list(toks, 2, 1)
#res$text
#res$id_unigram



*/

