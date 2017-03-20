//#include "dev.h"
#include "quanteda.h"
using namespace quanteda;

void count(Text text, 
           MapNgrams &counts_seq,
           IntParams &counts,
           const unsigned int &len_min, 
           const unsigned int &len_max){
    
    if (text.size() == 0) return; // do nothing with empty text
    text.push_back(0); // add padding to include last words
    Ngram tokens_seq;
    tokens_seq.reserve(text.size());
    
    // Collect sequence of specified types
    std::size_t len_text = text.size();
    for (std::size_t i = 0; i < len_text; i++) {
        if (text[i] == 0) continue;
        counts[text[i] - 1]++;
        for (std::size_t j = i; j < len_text; j++) {
            //Rcout << i << " " << j << "\n";
            unsigned int token = text[j];
            bool is_in;
            if (token == 0 || j - i >= len_max) {
                is_in = false;
            } else {
                is_in = true;
            }
            if (is_in) {
                //Rcout << "Match: " << token << "\n";
                tokens_seq.push_back(token);
            } else {
                //Rcout << "Not match: " <<  token << "\n";
                if (tokens_seq.size() > len_min) {
                    counts_seq[tokens_seq]++;
                }
                tokens_seq.clear();
                break;
            }
        }
    }
}

/*
struct count_mt : public Worker{
    
    Texts texts;
    const SetUnigrams &set_words;
    MapNgrams &counts_seq;
    const unsigned int &len_max;
    const bool &nested;
    
        
    count_mt(Texts texts_, SetUnigrams &set_words_, MapNgrams &counts_seq_, 
             const unsigned int &len_max_, const bool &nested_):
             texts(texts_), set_words(set_words_), counts_seq(counts_seq_), len_max(len_max_), nested(nested_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            count(texts[h], set_words, counts_seq, len_max, nested);
        }
    }
};

*/

void estimate(std::size_t i,
              VecNgrams &seqs,
              IntParams &counts,
              const int &count,
              IntParams &cs,
              DoubleParams &ss, 
              DoubleParams &ls, 
              const int &count_min){
    
    std::size_t n = seqs[i].size();
    if (n == 1) return; // ignore single words
    if (cs[i] < count_min) return;
    
    Ngram seq = seqs[i];
    
    // Calculate expected count here
    double c = std::log((long)count);
    double e = c;
    Rcout << "all: " << c << "\n";
    for (std::size_t j = 0; j < seq.size(); j++) {
        e += std::log((long)counts[seq[j] - 1]) - c;
    }
    Rcout << "e: " << e << "\n";
    double l = e;
    ss[i] = 0;
    ls[i] = l;
}

/*
struct estimate_mt : public Worker{
    
    VecNgrams &seqs;
    IntParams &cs;
    DoubleParams &ss;
    DoubleParams &ls;
    const unsigned int &count_min;
    const bool &ordered;
    
    // Constructor
    estimate_mt(VecNgrams &seqs_, IntParams &cs_, DoubleParams &ss_, DoubleParams &ls_, 
                const unsigned int &count_min_, const bool &ordered_):
                seqs(seqs_), cs(cs_), ss(ss_), ls(ls_), count_min(count_min_), 
                ordered(ordered_) {}

    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t i = begin; i < end; i++) {
            estimate(i, seqs, cs, ss, ls, count_min, ordered);
        }
    }
};
*/
 
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
 * @param ordered if true, use the Blaheta-Johnson method
 */

// [[Rcpp::export]]
DataFrame qatd_cpp_collocations(const List &texts_,
                                const CharacterVector types_,
                                const unsigned int count_min,
                                unsigned int len_min,
                                unsigned int len_max){

    Texts texts = as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);

    // Collect all sequences of specified words
    IntParams counts(types.size(), 0);
    MapNgrams counts_seq;
    //dev::Timer timer;
    //dev::start_timer("Count", timer);
// #if QUANTEDA_USE_TBB
//     count_mt count_mt(texts, set_words, counts_seq, len_max, nested);
//     parallelFor(0, texts.size(), count_mt);
// #else
    for (std::size_t h = 0; h < texts.size(); h++) {
        count(texts[h], counts_seq, counts, len_min, len_max);
    }
//endif
    //dev::stop_timer("Count", timer);
    
    // Separate map keys and values
    std::size_t len = counts_seq.size();
    VecNgrams seqs;
    IntParams cs;
    seqs.reserve(len);
    cs.reserve(len);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it) {
        seqs.push_back(it->first);
        cs.push_back(it->second);
    }
    
    // Normalize counts
    
    
    // Estimate significance of the sequences
    DoubleParams ss(len);
    DoubleParams ls(len);
//     //dev::start_timer("Estimate", timer);
// #if QUANTEDA_USE_TBB
//     estimate_mt estimate_mt(seqs, cs, ss, ls, count_min, ordered);
//     parallelFor(0, seqs.size(), estimate_mt);
// #else
    int sum = std::accumulate(counts.begin(), counts.end(), 0);
    for (std::size_t i = 0; i < seqs.size(); i++) {
        estimate(i, seqs, counts, sum, cs, ss, ls, count_min);
    }
// #endif
    //dev::stop_timer("Estimate", timer);

    DataFrame output_ = DataFrame::create(_["lambda"] = as<NumericVector>(wrap(ls)),
                                          _["sigma"] = as<NumericVector>(wrap(ss)),
                                          _["count"] = as<IntegerVector>(wrap(cs)));
    output_.attr("ids") = as<Tokens>(wrap(seqs));
    return output_;
}


/***R

toks <- tokens(data_corpus_inaugural)
#toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
out <- qatd_cpp_collocations(toks, attr(toks, 'types'), 1, 1, 2)

# out2$sequence <- lapply(out2$sequence, function(x) types[x])
# out2$str <- stringi::stri_c_list(out2$sequence, '_')
# out2$sequence <- NULL
# out2
# 
# out2$z <- out2$lambda / out2$sigma
# out2$p <- 1 - stats::pnorm(out2$z)





*/
