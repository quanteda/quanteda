//#include "dev.h"
#include "quanteda.h"
using namespace quanteda;

/* 
* This funciton is a orignal function by Watanabe, K (2016).
* The distribution of the match bit is more dense, tending to promote frequent sequences.
*/
int match_bit2(const std::vector<unsigned int> &tokens1, 
              const std::vector<unsigned int> &tokens2){
    
    std::size_t len1 = tokens1.size();
    std::size_t len2 = tokens2.size();
    int bit = 0;
    for (std::size_t i = 0; i < len1 && i < len2; i++) {
        if (tokens1[i] == tokens2[i]) bit += std::pow(2, i); // position dependent
    }
    return bit;
}

/* 
* This funciton is from Blaheta, D., & Johnson, M. (2001). 
* The distribution of the match bit is more sparse than in match_bit(), tending to promote rare sequences.
*/
int match_bit_ordered2(const std::vector<unsigned int> &tokens1, 
                      const std::vector<unsigned int> &tokens2){
    
    std::size_t len1 = tokens1.size();
    std::size_t len2 = tokens2.size();
    long bit = 0;
    for (std::size_t i = 0; i < len1 && i < len2; i++) {
        bit += (long)(tokens1[i] == tokens2[i]) * std::pow(2, i); // value in bit depends on positions
    }
    bit += (long)(len1 >= len2) * std::pow(2, len1); // for trailing space 
    return bit;
}

double sigma2(const std::vector<double> &counts){
    
    const std::size_t n = counts.size();
    const double base = n - 1;
    
    double s = 0.0;
    s += std::pow(base, 2) / counts[0];
    for (std::size_t b = 1; b < n - 1; b++) {
        s += 1.0 / counts[b];
    }
    s += 1.0 / counts[n - 1];
    return std::sqrt(s);
}

double lambda2(const std::vector<double> &counts){
    
    const std::size_t n = counts.size();
    
    double l = 0.0;
    l += std::log(counts[0]) * n - 1;
    for (std::size_t b = 1; b < n - 1; b++) {
        l -= std::log(counts[b]);
    }
    l += std::log(counts[n - 1]);
    return l;
}

void count2(Text text,
           MapNgrams &counts_seq,
           const unsigned int &len_max,
           const bool &nested){
    
    if (text.size() == 0) return; // do nothing with empty text
    text.push_back(0); // add padding to include last words
    Ngram tokens_seq;
    tokens_seq.reserve(text.size());
    
    // Collect sequence of specified types
    std::size_t len_text = text.size();
    for (std::size_t i = 0; i < len_text; i++) {
        for (std::size_t j = i; j < len_text; j++) {
            //Rcout << i << " " << j << "\n";
            unsigned int token = text[j];
            bool is_in = true;
            if (token == 0 || j - i >= len_max) {
                is_in = false;
            } 
            
            if (is_in) {
                //Rcout << "Match: " << token << "\n";
                tokens_seq.push_back(token);
            } else {
                //Rcout << "Not match: " <<  token << "\n";
                if (tokens_seq.size() > 1) {
                    counts_seq[tokens_seq]++;
                }
                tokens_seq.clear();
                if (!nested) i = j; // jump if nested is false
                break;
            }
        }
    }
}

struct count_mt2 : public Worker{
    
    Texts texts;
    MapNgrams &counts_seq;
    const unsigned int &len_max;
    const bool &nested;
    
    
    count_mt2(Texts texts_, MapNgrams &counts_seq_, 
             const unsigned int &len_max_, const bool &nested_):
        texts(texts_), counts_seq(counts_seq_), len_max(len_max_), nested(nested_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            count2(texts[h], counts_seq, len_max, nested);
        }
    }
};

void estimate2(std::size_t i,
              VecNgrams &seqs,
              IntParams &cs, 
              DoubleParams &ss, 
              DoubleParams &ls, 
              const int &count_min,
              const bool &ordered){
    
    std::size_t n = seqs[i].size();
    if (n == 1) return; // ignore single words
    if (cs[i] < count_min) return;
    std::vector<double> counts_bit;
    if (ordered) {
        counts_bit.resize(std::pow(2, n + 1), 0.5); // use 1/2 as smoothing
    } else {
        counts_bit.resize(std::pow(2, n), 0.5); // use 1/2 as smoothing
    }
    for (std::size_t j = 0; j < seqs.size(); j++) {
        if (i == j) continue; // do not compare with itself
        //if(ns[j] < count_min) continue; // this is different from the old vesion
        
        int bit;
        if (ordered) {
            bit = match_bit_ordered2(seqs[i], seqs[j]);
        } else {
            bit = match_bit2(seqs[i], seqs[j]);
        }
        counts_bit[bit] += cs[j];
    }
    counts_bit[std::pow(2, n)-1]  += cs[i] - 1;  // c(2^n-1) += number of itself  
    ss[i] = sigma2(counts_bit);
    ls[i] = lambda2(counts_bit);
}

struct estimate_mt2 : public Worker{
    
    VecNgrams &seqs;
    IntParams &cs;
    DoubleParams &ss;
    DoubleParams &ls;
    const unsigned int &count_min;
    const bool &ordered;
    
    // Constructor
    estimate_mt2(VecNgrams &seqs_, IntParams &cs_, DoubleParams &ss_, DoubleParams &ls_, 
                const unsigned int &count_min_, const bool &ordered_):
        seqs(seqs_), cs(cs_), ss(ss_), ls(ls_), count_min(count_min_), 
        ordered(ordered_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t i = begin; i < end; i++) {
            estimate2(i, seqs, cs, ss, ls, count_min, ordered);
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
* @param count_min sequences appear less than this are ignores
* @param nested if true, subsequences are also collected
* @param ordered if true, use the Blaheta-Johnson method
*/

// [[Rcpp::export]]
DataFrame qatd_cpp_sequences2(const List &texts_,
                             const CharacterVector &types_,
                             const unsigned int count_min,
                             unsigned int len_max,
                             bool nested,
                             bool ordered = false){
    
    Texts texts = as<Texts>(texts_);

    // Collect all sequences of specified words
    MapNgrams counts_seq;
    //dev::Timer timer;
    //dev::start_timer("Count", timer);
#if QUANTEDA_USE_TBB
    count_mt2 count_mt(texts, counts_seq, len_max, nested);
    parallelFor(0, texts.size(), count_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        count2(texts[h], counts_seq, len_max, nested);
    }
#endif
    //dev::stop_timer("Count", timer);
    
    // Separate map keys and values
    std::size_t len = counts_seq.size();
    VecNgrams seqs;
    IntParams cs, ns;
    seqs.reserve(len);
    cs.reserve(len);
    ns.reserve(len);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it) {
        seqs.push_back(it->first);
        cs.push_back(it->second);
        ns.push_back(it->first.size());
    }
    
    // Estimate significance of the sequences
    DoubleParams ss(len);
    DoubleParams ls(len);
    //dev::start_timer("Estimate", timer);
#if QUANTEDA_USE_TBB
    estimate_mt2 estimate_mt(seqs, cs, ss, ls, count_min, ordered);
    parallelFor(0, seqs.size(), estimate_mt);
#else
    for (std::size_t i = 0; i < seqs.size(); i++) {
        estimate2(i, seqs, cs, ss, ls, count_min, ordered);
    }
#endif
    //dev::stop_timer("Estimate", timer);
    
    // Convert sequences from integer to character
    CharacterVector seqs_(seqs.size());
    for (std::size_t i = 0; i < seqs.size(); i++) {
        seqs_[i] = join(seqs[i], types_, " ");
    }
    
    DataFrame output_ = DataFrame::create(_["collocation"] = seqs_,
                                          _["count"] = as<IntegerVector>(wrap(cs)),
                                          _["length"] = as<NumericVector>(wrap(ns)),
                                          _["lambda"] = as<NumericVector>(wrap(ls)),
                                          _["sigma"] = as<NumericVector>(wrap(ss)),
                                          _["stringsAsFactors"] = false);
    output_.attr("tokens") = as<Tokens>(wrap(seqs));
    return output_;
}


/***R

toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
types <- unique(as.character(toks))
types_upper <- types[stringi::stri_detect_regex(types, "^([A-Z][a-z\\-]{2,})")]

out2 <- qatd_cpp_sequences2(toks, types, 1, 2, TRUE, TRUE)
# out2$z <- out2$lambda / out2$sigma
# out2$p <- 1 - stats::pnorm(out2$z)


*/
