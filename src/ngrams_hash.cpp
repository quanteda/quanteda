#include <Rcpp.h>
#include <unordered_map>
#include <numeric>
#include "dev.h"
#include "quanteda.h"

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
    tthread::mutex mutex_id;
    tthread::mutex mutex_input;
    tthread::mutex mutex_output;
}
using namespace ngrams;

namespace std {
template <>

// Custom hash function for Ngram objects
    struct hash<Ngram> {
        std::size_t operator()(const Ngram &vec) const {
            unsigned int seed = std::accumulate(vec.begin(), vec.end(), 0);
            return std::hash<unsigned int>()(seed);
        }
    };
}

int ngram_id(Ngram ngram,
             std::unordered_map<Ngram, unsigned int> &map_ngram){

    // Add new ID without multiple access
    unsigned int &id_ngram = map_ngram[ngram];
    if(id_ngram){
        //Rcout << "Old " << id_ngram << ": ";
        //dev::print_ngram_hashed(ngram);

        return id_ngram;
    }
    mutex_id.lock();
    id_ngram = map_ngram.size();
    mutex_id.unlock();
    //Rcout << "New " << id_ngram << ": ";
    //dev::print_ngram_hashed(ngram);
    return id_ngram;
}

void skip_hashed(Text &tokens,
                 unsigned int start,
                 unsigned int n, 
                 std::vector<int> skips,
                 Ngram ngram,
                 Ngrams &ngrams,
                 std::unordered_map<Ngram, unsigned int> &map_ngram,
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
            skip_hashed(tokens, next, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams);
        }
    }else{
        
        ngrams[pos_ngrams] = ngram_id(ngram, map_ngram);
        
        //Rcout << "Add " << ngrams[pos_ngrams] << " at " << pos_ngrams << "/" << ngrams.size() << "\n";
        pos_tokens = 0;
        pos_ngrams++;
    }
}


Ngrams skipgram_hashed(Text tokens,
                       std::vector<int> ns, 
                       std::vector<int> skips,
                       std::unordered_map<Ngram, unsigned int> &map_ngram) {
    
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
            skip_hashed(tokens, start, n, skips, ngram, ngrams, map_ngram, pos_tokens, pos_ngrams); // Get ngrams as reference
        }
    }
    ngrams.resize(pos_ngrams);
    return ngrams;
}

// [[Rcpp::export]]
List qatd_cpp_ngram_hashed_vector(IntegerVector tokens_,
                                  IntegerVector ns_, 
                                  IntegerVector skips_){
    
    Text tokens = Rcpp::as< Text >(tokens_);
    std::vector<int> ns = Rcpp::as< std::vector<int> >(ns_);
    std::vector<int> skips = Rcpp::as< std::vector<int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    std::unordered_map<Ngram, unsigned int> map_ngram;
    Ngrams ngrams = skipgram_hashed(tokens, ns, skips, map_ngram);
    
    // Separate key and values of unordered_map
    List ids_unigram(map_ngram.size());
    for (std::pair<Ngram, unsigned int> iter : map_ngram){
        //Rcout << "ID " << to_string(iter.second) << ": ";
        //print_ngram_hashed(iter.first);
        ids_unigram[iter.second - 1] = iter.first;
    }
    
    return Rcpp::List::create(Rcpp::Named("ngram") = ngrams,
                              Rcpp::Named("id_unigram") = ids_unigram);
}

/* 
 * This funciton constructs ngrams from tokens object used in ngrams.tokens().
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @n size of ngramss
 * @skips size of skip (this has to be 1 for ngrams)
 * 
 */
// [[Rcpp::export]]
List qatd_cpp_ngram_hashed_list(List texts_,
                                IntegerVector ns_,
                                IntegerVector skips_) {
    
    Texts texts = Rcpp::as< Texts >(texts_);
    std::vector<int> ns = Rcpp::as< std::vector<int> >(ns_);
    std::vector<int> skips = Rcpp::as< std::vector<int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    std::unordered_map<Ngram, unsigned int> map_ngram;
    
    // Itterate over documents
    List texts_ngram(texts.size());
    for (int h = 0; h < texts.size(); h++){
        texts_ngram[h] = skipgram_hashed(texts[h], ns, skips, map_ngram);
        Rcpp::checkUserInterrupt(); // allow user to stop
    }
    
    // Separate key and values of unordered_map
    List ids_unigram(map_ngram.size());
    for (std::pair<Ngram, unsigned int> iter : map_ngram){
        //Rcout << "ID " << to_string(iter.second) << ": ";
        //print_ngram_hashed(iter.first);
        ids_unigram[iter.second - 1] = iter.first;
    }
    
    return Rcpp::List::create(Rcpp::Named("text") = texts_ngram,
                              Rcpp::Named("id_unigram") = ids_unigram);
}

struct skipgram_mt : public Worker{
    
    Texts &input;
    Texts &output;
    std::vector<int> ns;
    std::vector<int> skips;
    std::unordered_map<Ngram, unsigned int> &map_ngram;
    
    // Constructor
    skipgram_mt(Texts &input_, Texts &output_, std::vector<int> ns_, 
                std::vector<int> skips_, std::unordered_map<Ngram, unsigned int> &map_ngram_):
                input(input_), output(output_), ns(ns_), skips(skips_), map_ngram(map_ngram_){}
    
    // parallelFor calles this function with size_t
    Text temp_in, temp_out;
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (int h = begin; h < end; h++){
            /*
            mutex_input.lock();
            temp_in = input[h];
            mutex_input.unlock();
            temp_out = skipgram_hashed(temp_in, ns, skips, map_ngram);
            mutex_output.lock();
            output[h] = temp_out;
            mutex_output.unlock();
            */
            output[h] = skipgram_hashed(input[h], ns, skips, map_ngram);
        }
    }
    
};

/*
 * This funciton constructs multi-thrad version of qatd_cpp_ngram_hashed_list.
 * @creator Kohei Watanabe
 */

// [[Rcpp::export]]
List qatd_cpp_ngram_mt_list(List texts_,
                            IntegerVector ns_,
                            IntegerVector skips_) {
    
    Texts input = Rcpp::as< Texts >(texts_);
    std::vector<int> ns = Rcpp::as< std::vector<int> >(ns_);
    std::vector<int> skips = Rcpp::as< std::vector<int> >(skips_);
    
    // Register both ngram (key) and unigram (value) IDs in a hash table
    std::unordered_map<Ngram, unsigned int> map_ngram;
    
    Texts ouput(input.size());
    skipgram_mt skipgram_mt(input, ouput, ns, skips, map_ngram);
    
    // call parallelFor to do the work
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


/* 
 * This funciton constructs character expressions of ngrams from set of ids and original types,
 * but not so stable. Should be removed in the future.
 * @creator Kohei Watanabe
 */
CharacterVector qatd_cpp_ngram_unhash_type(ListOf<IntegerVector> ids_ngram, 
                                           CharacterVector types, String delim){
    types.push_front(""); // offset types to match index in C++
    CharacterVector tokens_ngram(ids_ngram.size());
    for(int i=0; i < ids_ngram.size(); i++){
        tokens_ngram[i] = join(types[ids_ngram[i]], delim);
    }
    return tokens_ngram;
}






/*** R

library(quanteda)
# txt <- c('a b c d e', 'c d e f g')
# toks <- tokens(txt, what='fastestword')
# res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
# res <- qatd_cpp_ngram_hashed_list(toks, 2, 1)
# res$text
# res$id_unigram

RcppParallel::setThreadOptions(numThreads = 4)
toks = rep(list(1:1000, 1001:2000), 10)
res <- qatd_cpp_ngram_mt_list(toks, 2, 1)
#res <- qatd_cpp_ngram_hashed_list(toks, 2, 1)
res$text
res$id_unigram



*/

