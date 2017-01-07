#include <Rcpp.h>
//#include "dev.h"
#include "quanteda.h"

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
          Text &tokens_ng,
          unsigned int start,
          unsigned int n, 
          std::vector<unsigned int> skips,
          Ngram ngram,
          MapNgrams &map_ngram,
          unsigned int pos_ngram, 
          unsigned int &pos_ngrams) {
    
    
    ngram[pos_ngram] = tokens[start];
    pos_ngram++;
    
    //Rcout << "Size " << tokens.size() << "\n";
    //Rcout << "Token " << tokens[start] << "\n";
    
    if(pos_ngram < n){
        for (std::size_t j = 0; j < skips.size(); j++){
            unsigned int next = start + skips[j];
            if(tokens.size() - 1 < next) break;
            if(tokens[next] == 0) break; // Skip padding
            //Rcout << "Join " << tokens[start] << " at " << start << " with " << next << "\n";
            skip(tokens, tokens_ng, next, n, skips, ngram, map_ngram, pos_ngram, pos_ngrams);
        }
    }else{
        tokens_ng[pos_ngrams] = ngram_id(ngram, map_ngram);
        
        //Rcout << "Add " << tokens_ng[pos_ngrams] << " at " << pos_ngrams << "/" << tokens_ng.size() << "\n";
        pos_ngram = 0;
        pos_ngrams++;
    }
}


Text skipgram(Text tokens,
              std::vector<unsigned int> ns, 
              std::vector<unsigned int> skips,
              MapNgrams &map_ngram) {
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int pos_ngram = 0; // position in ngram
    unsigned int pos_ngrams = 0; // position in ngrams
    
    // Pre-allocate memory
    int size_reserve = 0;
    for (std::size_t k = 0; k < ns.size(); k++) {
        size_reserve += std::pow(skips.size(), ns[k]) * tokens.size();
    }
    Text tokens_ng(size_reserve);

    // Generate skipgrams recursively
    for (std::size_t k = 0; k < ns.size(); k++) {
        unsigned int n = ns[k];
        if (tokens.size() < n) continue;
        Ngram ngram(n);
        for (std::size_t start = 0; start < tokens.size() - (n - 1); start++) {
            if(tokens[start] == 0) continue; // skip padding
            skip(tokens, tokens_ng, start, n, skips, ngram, map_ngram, pos_ngram, pos_ngrams); // Get ngrams as reference
        }
    }
    tokens_ng.resize(pos_ngrams);
    return tokens_ng;
}

struct skipgram_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<unsigned int> ns;
    const std::vector<unsigned int> skips;
    MapNgrams &map_ngram;
    
    // Constructor
    skipgram_mt(Texts &input_, Texts &output_, std::vector<unsigned int> ns_, std::vector<unsigned int> skips_, 
                MapNgrams &map_ngram_):
                input(input_), output(output_), ns(ns_), skips(skips_), map_ngram(map_ngram_){}
    
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            output[h] = skipgram(input[h], ns, skips, map_ngram);
        }
    }
};

/* 
 * This funciton generates ngrams/skipgrams from tokens object. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_ngrams()
 * @creator Kohei Watanabe
 * @param types_ types of tokens
 * @param texts_ tokens ojbect
 * @param delim_ string to join words
 * @param ns_ size of ngramss
 * @param skips_ size of skip (this has to be 1 for ngrams)
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_ngrams(List texts_,
                            CharacterVector types_,
                            String delim_,
                            IntegerVector ns_,
                            IntegerVector skips_) {
    
    Texts input = Rcpp::as< Texts >(texts_);
    std::string delim = delim_;
    std::vector<std::string> types = Rcpp::as< std::vector<std::string> >(types_);
    std::vector<unsigned int> ns = Rcpp::as< std::vector<unsigned int> >(ns_);
    std::vector<unsigned int> skips = Rcpp::as< std::vector<unsigned int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    MapNgrams map_ngram;
    
    // dev::Timer timer;
    // dev::start_timer("Ngram generation", timer);
    Texts output(input.size());
    #if RCPP_PARALLEL_USE_TBB
    skipgram_mt skipgram_mt(input, output, ns, skips, map_ngram);
    parallelFor(0, input.size(), skipgram_mt);
    #else
    for (std::size_t h = 0; h < input.size(); h++){
        output[h] = skipgram(input[h], ns, skips, map_ngram);
    }
    #endif
    // dev::stop_timer("Ngram generation", timer);
    
    // dev::start_timer("Token generation", timer);
    // Create character tokens from unordered_map
    std::vector<std::string> types_ngram(map_ngram.size());
    
    for (std::pair<Ngram, unsigned int> it : map_ngram){
        std::string type_ngram = types[it.first[0] - 1];
        for (std::size_t i = 1; i < it.first.size(); i++){
            type_ngram += delim + types[it.first[i] - 1];
        }
        types_ngram[it.second - 1] = type_ngram;
    }
    
    // dev::stop_timer("Token generation", timer);
    
    // Return IDs as attribute
    //ListOf<IntegerVector> texts_ngram = Rcpp::wrap(output);
    List texts_ngram = as_list(output);
    texts_ngram.attr("types") = types_ngram;

    return texts_ngram;

}


/*** R

txt <- c('a b c d e', 'c d e f g')
#txt <- readLines('~/Documents/Brexit/Analysis/all_bbc_2015.txt') # 80MB
tok <- quanteda::tokens(txt)

RcppParallel::setThreadOptions(4)
res <- qatd_cpp_tokens_ngrams(tok, attr(tok, 'types'), "-", 2, 1)
str(res)

tok <- quanteda::tokens(data_corpus_inaugural)
out <- qatd_cpp_tokens_ngrams(unclass(tok), attr(tok, 'types'), "_", 2, 1)

#RcppParallel::setThreadOptions(4)
#toks = rep(list(1:1000, 1001:2000), 10)
#toks = rep(list(1:10000, 10001:20000), 10000)
#res <- qatd_cpp_tokens_ngrams(toks, 2, 1)
#res$text
#res$id_unigram



*/

