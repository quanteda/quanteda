#include <Rcpp.h>
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
            //unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
            //return std::hash<unsigned int>()(seed);
            unsigned int seed = 0;
            for(int i = 0; i < vec.size(); i++){
                //seed += vec[i] * std::pow(10, i) ;
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

/*
 *  This function specializes in ngrams but there is no performance gain. 
 */
Ngrams ngram(Text tokens,
             std::vector<int> ns, 
             tbb::concurrent_unordered_map<Ngram, unsigned int, hash_ngram, equal_ngram> &map_ngram) {
    
    int pos_ngrams = 0; // Position in ngrams
    
    // Pre-allocate memory
    int size_reserve = tokens.size();
    Ngrams ngrams(size_reserve);
    
    // Generate ngrams
    for (int k = 0; k < ns.size(); k++) {
        int n = ns[k];
        for(int start = 0; start < tokens.size() - (n - 1); start++){
            Ngram ngram(tokens.begin() + start, tokens.begin() + start + n);
            ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
            //Rcout << "Add " << ngrams[pos_ngrams] << " at " << ngrams.size() << "\n";
            pos_ngrams++;
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

    Texts output(input.size());
    skipgram_mt skipgram_mt(input, output, ns, skips, map_ngram);
    
    // Apply skipgram_mt to blocked ranges
    dev::Timer timer;
    dev::start_timer("Ngram generation", timer);
    parallelFor(0, input.size(), skipgram_mt);
    dev::stop_timer("Ngram generation", timer);
    
    dev::start_timer("ID extraction", timer);
    // Separate key and values of unordered_map
    std::vector< std::vector<unsigned int> > ids(map_ngram.size(), std::vector<unsigned int>(1, 0)); // set default value to aviode NULL
    for (std::pair<Ngram, unsigned int> it : map_ngram){
        //Rcout << "ID " << to_string(it.second) << ": ";
        //print_ngram_hashed(it.first);
        ids[it.second - 1] = it.first;
    }
    dev::stop_timer("ID extraction", timer);
    
    // Return IDs as attribute
    ListOf<IntegerVector> texts_ngram = Rcpp::wrap(output);
    ListOf<IntegerVector> ids_unigram = Rcpp::wrap(ids);
    texts_ngram.attr("ids") = ids_unigram;
    return texts_ngram;

}


/*** R

#txt <- c('a b c d e', 'c d e f g')
#txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
#tok <- tokens(txt)

RcppParallel::setThreadOptions(2)
res <- qatd_cpp_ngram_mt_list(tok, 2, 1)
#res

#RcppParallel::setThreadOptions(4)
#toks = rep(list(1:1000, 1001:2000), 10)
#toks = rep(list(1:10000, 10001:20000), 10000)
#res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
#res$text
#res$id_unigram



*/

