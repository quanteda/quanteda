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


typedef tbb::concurrent_unordered_set<unsigned int> SetTypes;
    
int match_bit(const std::vector<unsigned int> &tokens1, 
              const std::vector<unsigned int> &tokens2){
  
    int len1 = tokens1.size();
    int len2 = tokens2.size();
    int len = std::min(len1, len2);
    long bit = 0; // for Solaris
    for (int i = 0; i < len; i++){
        bit += tokens1[i] == tokens2[i];
    }
    bit += len1 >= len2; // add one point for trailing space 
    return bit;
}

double sigma(std::vector<long> &counts, int n){
  
    double s = 0;
    for (int b = 1; b <= n; b++){
        s += 1.0 / counts[b];
    }
    double base = n - 1; // for Solaris
    s += std::pow(base, 2) / counts[0];
    return std::sqrt(s);
}

double lambda(std::vector<long> counts, int n){
  
    double l = std::log(counts[n]);
    for (int b = 1; b < n; b++){
        l -= std::log(counts[b]);
    }
    l += (n - 1) * std::log(counts[0]);
    return l;
}

MapNgrams count(Text text, 
                SetTypes &set_types,
                MapNgrams &counts_seq,
                bool nested){
    
    //Rcout << "Text " << h << ": ";
    //dev::print_ngram(text);
    
    text.push_back(0); // add padding to include last words
    Ngram tokens_seq;
    
    size_t len_text = text.size();
    for (size_t i = 0; i < len_text; i++){ // scan texts ignoring first words in texts
        for (size_t j = i; j < len_text; j++){ // collect nested sequence starting from i
            //Rcout << i << " " << j << "\n";
            unsigned int token = text[j];
            bool is_in;
            if(token == 0){
                is_in = false;
            }else{
                is_in = set_types.find(token) != set_types.end();
            }
            if (is_in){
                //Rcout << "Match: " << token << "\n";
                tokens_seq.push_back(token);
            }else{
                //Rcout << "Not match: " <<  token << "\n";
                if(tokens_seq.size() > 1){
                    counts_seq[tokens_seq]++;
                    //Rcout << "Sequence: ";
                    //dev::print_ngram(tokens_seq);
                }
                tokens_seq.clear();
                if (!nested) i = j; // jump if nested is false
                break;
            }
        }
    }
    
    
    return counts_seq;
} 

// [[Rcpp::export]]

List qutd_cpp_sequences(List texts_,
                        NumericVector words_,
                        int count_min,
                        bool nested){
    
    //Texts input = Rcpp::as<Texts>(texts_);
    Texts texts = Rcpp::as<Texts>(texts_);
    std::vector<unsigned int> words = Rcpp::as< std::vector<unsigned int> >(words_);
    
    //Rcpp::List texts(x);
    //std::map<std::vector<std::string>, int> counts_seq; // unorderd_map cannot take vector as key
    MapNgrams counts_seq;
    SetTypes set_types (words.begin(), words.end());
    
    // Find all sequences of specified types
    for (size_t h = 0; h < texts.size(); h++){
        Text text = texts[h];
        counts_seq = count(text, set_types, counts_seq, nested);
    }
  
    // Find significance of sequences
  
    size_t n = counts_seq.size();
    tbb::concurrent_vector<Ngram> seqs;
    seqs.reserve(n);
    tbb::concurrent_vector<int> ns;
    ns.reserve(n);
    
    // Separate map keys and values
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it){
        seqs.push_back(it->first);
        ns.push_back(it->second);
    }
    
    // Estimation
    tbb::concurrent_vector<double> ss(n);
    tbb::concurrent_vector<double> ls(n);
    
    //for(auto it1 = counts_seq.begin(); it1 != counts_seq.end(); ++it1 ){
    for(size_t i = 0; i < n; i++){
        size_t len = seqs[i].size();
        if(len < 2) continue; // ignore single words
        if(ns[i] < count_min) continue;
        std::vector<long> counts_bit(len + 1, 1); // add one smoothing
        //for(auto it2 = counts_seq.begin(); it2 != counts_seq.end(); ++it2 ){
        for(size_t j = 0; j < n; j++){
            if(ns[j] < count_min) continue;
            if(i == j) continue; // do not compare with itself
            int bit = match_bit(seqs[i], seqs[j]);
            counts_bit[bit] += ns[i];
        }
        ss[i] = sigma(counts_bit, len);
        ls[i] = lambda(counts_bit, len);
    }
    
    Rcpp::List sequences;
    NumericVector lambdas;
    NumericVector sigmas;
    NumericVector counts;
    for(size_t k = 0; k < n; k++){
        if(ns[k] < count_min) continue;
        sequences.push_back(seqs[k]);
        lambdas.push_back(ls[k]);
        sigmas.push_back(ss[k]);
        counts.push_back(ns[k]);
    }
        
    return Rcpp::List::create(Rcpp::Named("sequence") = sequences,
                              Rcpp::Named("lambda") = lambdas,
                              Rcpp::Named("sigma") = sigmas,
                              Rcpp::Named("count") = counts
    );

  
}


/***R
library(quanteda)
toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
types <- unique(as.character(toks))
types_upper <- types[stringi::stri_detect_regex(types, "^([A-Z][a-z\\-]{2,})")]

out2 <- qutd_cpp_sequences(toks, match(types_upper, types), 4, TRUE)
out2$sequence <- lapply(out2$sequence, function(x) types[x])
out2$str <- stringi::stri_c_list(out2$sequence, '_')
out2$sequence <- NULL
out2

out2$z <- out2$lambda / out2$sigma
out2$p <- 1 - stats::pnorm(out2$z)

merge(as.data.frame(out), as.data.frame(out2),  by='str')

*/
