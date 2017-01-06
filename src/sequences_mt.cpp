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


int match_bit(const std::vector<unsigned int> &tokens1, 
              const std::vector<unsigned int> &tokens2){
  
    size_t len1 = tokens1.size();
    size_t len2 = tokens2.size();
    long bit = 0; // for Solaris
    for (int i = 0; i < len1 && i < len2; i++){
        bit += tokens1[i] == tokens2[i];
    }
    bit += len1 >= len2; // add one point for trailing space 
    return bit;
}

double sigma(std::vector<long> &counts, int n){
  
    double s = 0;
    for (size_t b = 1; b <= n; b++){
        s += 1.0 / counts[b];
    }
    double base = n - 1; // for Solaris
    s += std::pow(base, 2) / counts[0];
    return std::sqrt(s);
}

double lambda(std::vector<long> counts, int n){
  
    double l = std::log(counts[n]);
    for (size_t b = 1; b < n; b++){
        l -= std::log(counts[b]);
    }
    l += (n - 1) * std::log(counts[0]);
    return l;
}

void count(Text text, 
           SetUnigrams &set_words, 
           MapNgrams &counts_seq, 
           bool nested){
    
    if(text.size() == 0) return; // do nothing with empty text
    text.push_back(0); // add padding to include last words
    Ngram tokens_seq;
    
    // Collect sequence of specified types
    size_t len_text = text.size();
    for (size_t i = 0; i < len_text; i++){
        for (size_t j = i; j < len_text; j++){
            //Rcout << i << " " << j << "\n";
            unsigned int token = text[j];
            bool is_in;
            if (token == 0){
                is_in = false;
            }else{
                is_in = set_words.find(token) != set_words.end();
            }
            if (is_in){
                //Rcout << "Match: " << token << "\n";
                tokens_seq.push_back(token);
            }else{
                //Rcout << "Not match: " <<  token << "\n";
                if(tokens_seq.size() > 1){
                    counts_seq[tokens_seq]++;
                }
                tokens_seq.clear();
                if (!nested) i = j; // jump if nested is false
                break;
            }
        }
    }
}

struct count_mt : public Worker{
    
    Texts texts;
    SetUnigrams &set_words;
    MapNgrams &counts_seq;
    bool nested;
        
    count_mt(Texts texts_, SetUnigrams &set_words_, MapNgrams &counts_seq_, bool nested_):
             texts(texts_), set_words(set_words_), counts_seq(counts_seq_), nested(nested_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (int h = begin; h < end; h++){
            count(texts[h], set_words, counts_seq, nested);
        }
    }
};

struct estimate_mt : public Worker{
    
    VecNgrams &seqs;
    tbb::concurrent_vector<int> &ns;
    tbb::concurrent_vector<double> &ss;
    tbb::concurrent_vector<double> &ls;
    int count_min;
    
    // Constructor
    estimate_mt(VecNgrams &seqs_, tbb::concurrent_vector<int> &ns_, 
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
                //if(ns[j] < count_min) continue; // this is different from the old vesion
                int bit = match_bit(seqs[i], seqs[j]);
                counts_bit[bit] += ns[i];
            }
            ss[i] = sigma(counts_bit, n);
            ls[i] = lambda(counts_bit, n);
        }
    }
};

/* 
 * This funciton estimate the strength of association between specified words 
 * that appear in sequences. Estimates are slightly different from the old version,
 *  because this faster version does not ignore infrequent sequences.
 * @used sequences()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ types of tokens in sequences
 * @param count_min sequences appear less than this are ignores
 * @param nested if true, subsequences are also collected
 * 
 */

// [[Rcpp::export]]
List qutd_cpp_sequences(List texts_,
                        NumericVector words_,
                        int count_min,
                        bool nested){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    std::vector<unsigned int> words = Rcpp::as< std::vector<unsigned int> >(words_);
    SetUnigrams set_words (words.begin(), words.end());
    
    // Collect all sequences of specified words
    MapNgrams counts_seq;
    count_mt count_mt(texts, set_words, counts_seq, nested);
    
    //dev::Timer timer;
    //dev::start_timer("Count", timer);
    parallelFor(0, texts.size(), count_mt);
    //dev::stop_timer("Count", timer);
    
    // Separate map keys and values
    size_t len = counts_seq.size();
    VecNgrams seqs;
    tbb::concurrent_vector<int> ns;
    seqs.reserve(len);
    ns.reserve(len);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it){
        seqs.push_back(it->first);
        ns.push_back(it->second);
    }
    
    // Estimate significance of the sequences
    tbb::concurrent_vector<double> ss(len);
    tbb::concurrent_vector<double> ls(len);
    estimate_mt estimate_mt(seqs, ns, ss, ls, count_min);
    
    //dev::start_timer("Estimate", timer);
    parallelFor(0, seqs.size(), estimate_mt);
    //dev::stop_timer("Estimate", timer);
    
    //dev::start_timer("Convert", timer);
    // Convert to Rcpp objects
    Rcpp::List sequences(len);
    NumericVector lambdas(len);
    NumericVector sigmas(len);
    NumericVector counts(len);
    for(size_t k = 0; k < len; k++){
        sequences[k] = seqs[k];
        lambdas[k] = ls[k];
        sigmas[k] = ss[k];
        counts[k] = ns[k];
    }
    //dev::stop_timer("Convert", timer);
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

# df <- merge(as.data.frame(out), as.data.frame(out2),  by='str')
# df[order(df$p.y),]
# 
# microbenchmark::microbenchmark(
#     qutd_cpp_sequences(toks, match(types_upper, types), 1, TRUE),
#     find_sequence_cppl(as.tokenizedTexts(toks), types_upper, 1, 0.001, TRUE), times=10
# )



*/
