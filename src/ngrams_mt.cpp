#include <Rcpp.h>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;


int ngram_id(Ngram ngram,
             MapNgrams &map_ngram){
    
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
          MapNgrams &map_ngram,
          int pos_ngram, int &pos_ngrams) {
    
    
    ngram[pos_ngram] = tokens[start];
    pos_ngram++;
    
    //Rcout << "Size " << tokens.size() << "\n";
    //Rcout << "Token " << tokens[start] << "\n";
    
    if(pos_ngram < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next < 0 || tokens.size() - 1 < next) break;
            if(tokens[next] == 0) break; // Skip padding
            //Rcout << "Join " << tokens[start] << " at " << start << " with " << next << "\n";
            skip(tokens, next, n, skips, ngram, ngrams, map_ngram, pos_ngram, pos_ngrams);
        }
    }else{
        ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
        
        //Rcout << "Add " << ngrams[pos_ngrams] << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
        pos_ngram = 0;
        pos_ngrams++;
    }
}


Ngrams skipgram(Text tokens,
                std::vector<int> ns, 
                std::vector<int> skips,
                MapNgrams &map_ngram) {
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    int pos_ngram = 0; // position in ngram
    int pos_ngrams = 0; // position in ngrams
    
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
            if(tokens[start] == 0) continue; // skip padding
            skip(tokens, start, n, skips, ngram, ngrams, map_ngram, pos_ngram, pos_ngrams); // Get ngrams as reference
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
    MapNgrams &map_ngram;
    
    // Constructor
    skipgram_mt(Texts &input_, Texts &output_, std::vector<int> ns_, std::vector<int> skips_, 
                MapNgrams &map_ngram_):
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
                            CharacterVector types_,
                            String delim_,
                            IntegerVector ns_,
                            IntegerVector skips_) {
    
    Texts input = Rcpp::as< Texts >(texts_);
    std::string delim = delim_;
    std::vector<std::string> types = Rcpp::as< std::vector<std::string> >(types_);
    std::vector<int> ns = Rcpp::as< std::vector<int> >(ns_);
    std::vector<int> skips = Rcpp::as< std::vector<int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    MapNgrams map_ngram;

    Texts output(input.size());
    skipgram_mt skipgram_mt(input, output, ns, skips, map_ngram);
    
    // Apply skipgram_mt to blocked ranges
    dev::Timer timer;
    dev::start_timer("Ngram generation", timer);
    parallelFor(0, input.size(), skipgram_mt);
    dev::stop_timer("Ngram generation", timer);
    
    dev::start_timer("Token generation", timer);
    // Create character tokens from unordered_map
    std::vector<std::string> types_ngram(map_ngram.size());
    
    for (std::pair<Ngram, unsigned int> it : map_ngram){
        std::string type_ngram = types[it.first[0] - 1];
        for(int i = 1; i < it.first.size(); i++){
            type_ngram += delim + types[it.first[i] - 1];
        }
        types_ngram[it.second - 1] = type_ngram;
    }
    
    dev::stop_timer("Token generation", timer);
    
    // Return IDs as attribute
    ListOf<IntegerVector> texts_ngram = Rcpp::wrap(output);
    texts_ngram.attr("types") = types_ngram;
    texts_ngram.attr("class") = "tokens";

    return texts_ngram;

}


/*** R

txt <- c('a b c d e', 'c d e f g')
#txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
tok <- quanteda::tokens(txt)

RcppParallel::setThreadOptions(1)
res <- qatd_cpp_ngram_mt_list(tok, attr(tok, 'types'), "-", 2, 1)
str(res)

#RcppParallel::setThreadOptions(4)
#toks = rep(list(1:1000, 1001:2000), 10)
#toks = rep(list(1:10000, 10001:20000), 10000)
#res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
#res$text
#res$id_unigram



*/

