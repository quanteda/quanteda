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

struct count_mt : public Worker{
    
    Texts texts;
    SetTypes &set_types;
    MapNgrams &counts_seq;
    bool nested;
        
    count_mt(Texts texts_, SetTypes &set_types_, MapNgrams &counts_seq_, bool nested_):
             texts(texts_), set_types(set_types_), counts_seq(counts_seq_), nested(nested_) {}
    
    //Rcout << "Text " << h << ": ";
    //dev::print_ngram(text);
    void operator()(std::size_t begin, std::size_t end){

        for (int h = begin; h < end; h++){
            Text text = texts[h];
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
        }
    }
};

struct estimate_mt : public Worker{
    
    tbb::concurrent_vector<Ngram> &seqs;
    tbb::concurrent_vector<int> &ns;
    tbb::concurrent_vector<double> &ss;
    tbb::concurrent_vector<double> &ls;
    int count_min;
    
    // Constructor
    estimate_mt(tbb::concurrent_vector<Ngram> &seqs_, tbb::concurrent_vector<int> &ns_, 
                tbb::concurrent_vector<double> &ss_, tbb::concurrent_vector<double> &ls_, int count_min_):
                seqs(seqs_), ns(ns_), ss(ss_), ls(ls_), count_min(count_min_) {}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        size_t len = seqs.size();
        for (int i = begin; i < end; i++){
            size_t n = seqs[i].size();
            if(n == 1) continue; // ignore single words
            if(ns[i] < count_min) continue;
            std::vector<long> counts_bit(len + 1, 1); // add one smoothing
            for(size_t j = 0; j < len; j++){
                if(i == j) continue; // do not compare with itself
                //if(ns[j] < count_min) continue; // this is differet from old vesion
                int bit = match_bit(seqs[i], seqs[j]);
                counts_bit[bit] += ns[i];
            }
            ss[i] = sigma(counts_bit, n);
            ls[i] = lambda(counts_bit, n);
        }
    }
};

// [[Rcpp::export]]

List qutd_cpp_sequences(List texts_,
                        NumericVector words_,
                        int count_min,
                        bool nested){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    std::vector<unsigned int> words = Rcpp::as< std::vector<unsigned int> >(words_);
    
    // Hash tables
    MapNgrams counts_seq;
    SetTypes set_types (words.begin(), words.end());
    
    count_mt count_mt(texts, set_types, counts_seq, nested);
    
    dev::Timer timer;
    dev::start_timer("Count", timer);
    parallelFor(0, texts.size(), count_mt);
    dev::stop_timer("Count", timer);
    
    // Separate map keys and values
    size_t len = counts_seq.size();
    tbb::concurrent_vector<Ngram> seqs;
    seqs.reserve(len);
    tbb::concurrent_vector<int> ns;
    ns.reserve(len);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it){
        seqs.push_back(it->first);
        ns.push_back(it->second);
    }
    
    // Estimate significance of the sequences
    tbb::concurrent_vector<double> ss(len);
    tbb::concurrent_vector<double> ls(len);
    estimate_mt estimate_mt(seqs, ns, ss, ls, count_min);
    
    dev::start_timer("Estimate", timer);
    parallelFor(0, seqs.size(), estimate_mt);
    dev::stop_timer("Estimate", timer);
    
    // Convert to Rcpp objects
    Rcpp::List sequences;
    NumericVector lambdas;
    NumericVector sigmas;
    NumericVector counts;
    for(size_t k = 0; k < len; k++){
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

out2 <- qutd_cpp_sequences(toks, match(types_upper, types), 1, TRUE)
out2$sequence <- lapply(out2$sequence, function(x) types[x])
out2$str <- stringi::stri_c_list(out2$sequence, '_')
out2$sequence <- NULL
out2

out2$z <- out2$lambda / out2$sigma
out2$p <- 1 - stats::pnorm(out2$z)

df <- merge(as.data.frame(out), as.data.frame(out2),  by='str')
df[order(df$p.y),]
*/
