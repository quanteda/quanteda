#include "quanteda.h"
#include "dev.h"
#include <bitset>
using namespace quanteda;

#if QUANTEDA_USE_TBB
typedef tbb::concurrent_vector<std::pair<Ngram, UintParam>> VecPair;
typedef tbb::concurrent_unordered_map<Ngram, std::pair<UintParam, UintParam>, hash_ngram, equal_ngram> MapNgramsPair;
#else
typedef std::vector<std::pair<Ngram, UintParam>> VecPair;
typedef std::unordered_map<Ngram, std::pair<unsigned int, unsigned int>, hash_ngram, equal_ngram> MapNgramsPair;
#endif    

// return the matching pattern between two words at each position, 0 for matching, 1 for not matching.
// for example, for 3-gram, bit = 000, 001, 010 ... 111 eg. 0-7
int match_bit2(const std::vector<unsigned int> &tokens1, 
               const std::vector<unsigned int> &tokens2){
    
    std::size_t len1 = tokens1.size();
    std::size_t len2 = tokens2.size();
    int bit = 0;
    for (std::size_t i = 0; i < len1 && i < len2; i++) {
        if (tokens1[i] == tokens2[i]) bit += std::pow(2, i); // position dependent, bit=0:(2^n-1)
    }
    return bit;
}

// unigram subtuples from B&J algorithm -- lambda1
double sigma_uni(const std::vector<double> &counts, const std::size_t ntokens){
    double s = 0.0;
    s += std::pow(ntokens - 1, 2) / counts[0];
    for (std::size_t b = 0; b < ntokens; b++) {
        s += 1.0 / counts[std::pow(2, b)];
    }
    s += 1.0 / counts[std::pow(2, ntokens) - 1];
    return std::sqrt(s);
}

double lambda_uni(const std::vector<double> &counts, const std::size_t ntokens){
    double l = 0.0;
    l += std::log(counts[0]) * (ntokens - 1); // c0
    for (std::size_t b = 0; b < ntokens; b++) {  //c(b), #(b)=1
        l -= std::log(counts[std::pow(2, b)]);
    }
    l += std::log(counts[std::pow(2, ntokens) - 1]); //c(2^n-1)
    return l;
}

// all subtuples from B&J algorithm
double sigma_all(const std::vector<double> &counts){
    const std::size_t n = counts.size();
    double s = 0.0;
    
    for (std::size_t b = 0; b < n; b++) {
        s += 1.0 / counts[b];
    }
    
    return std::sqrt(s);
}

double lambda_all(const std::vector<double> &counts, const std::size_t ntokens){
    const std::size_t n = counts.size();
    
    double l = 0.0;
    
    for (std::size_t b = 0; b < n; b++) {  //c(b), #(b)=1
        std::bitset<8> bitb(b);
        l += std::pow(-1, ntokens - bitb.count()) * std::log(counts[b]);
    }
    
    return l;
}

//calculate dice coefficients
// dice = 2*C(2^n-1)/sum(i=1:2^n-1)(#(i)*C(i)): #(i) counts number of digit'1'
double compute_dice(const std::vector<double> &counts){
    double dice = 0.0;
    const std::size_t n = counts.size();
    for (std::size_t b = 1; b < n; b++) {  //c(b), #(b)=1
        std::bitset<8> bitb(b);
        dice += bitb.count() * counts[b]; 
    }
    //Rcout<<"counts[n-1]="<<counts[n-1]<<"dice="<<dice<<std::endl;
    dice = counts[n-1]/(dice);  // smoothing has been applied when declaring counts_bit[]
    return dice;
}

Text replace(Text tokens, 
             SetUnigrams &set_ignore,
             unsigned int &id_mark){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    for (std::size_t i = 0; i < tokens.size(); i++) {
        auto it = set_ignore.find(tokens[i]);
        if (it != set_ignore.end()) {
            tokens[i] = id_mark;
        }
    }
    return tokens;
}

struct replace_mt : public Worker{
    
    Texts &texts;
    SetUnigrams &set_ignore;
    unsigned int &id_mark;
    
    replace_mt(Texts &texts_, SetUnigrams &set_ignore_, unsigned int &id_mark_):
        texts(texts_), set_ignore(set_ignore_), id_mark(id_mark_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++) {
            texts[h] = replace(texts[h], set_ignore, id_mark);
        }
    }
};

void counts(Text text,
            MapNgramsPair &counts_seq,
            const std::vector<unsigned int> &sizes,
            const unsigned int &id_mark){
    
    std::vector<bool> flags_nested(text.size(), false);
    std::vector<bool> flags_temp(text.size(), false);
    
    for (auto size : sizes) { // start from the largest size
        if (text.size() < size) continue;
        for (std::size_t i = 0; i < text.size() - size + 1; i++) {
            
            bool marked = false; // if the segment contain ineligible
            bool nested = false;
            bool padded = false;
            
            // check if sub-vector will contain ineligible
            for (std::size_t j = i; j <  i + size; j++) {
                if (text[j] == 0) {
                    padded = true;
                } else if (text[j] == id_mark) {
                    marked = true;
                    i = j; // jump
                    break;
                }
                if (flags_nested[j]) {
                    nested = true;
                }
            }
            
            if (!marked) {
                Text text_sub(text.begin() + i, text.begin() + i + size);
                //Rcout << "@" << i << " " <<  nested << ": ";
                //dev::print_ngram(text_sub);
                auto &count = counts_seq[text_sub];
                count.first++;
                if (!padded) {
                    if (nested) {
                        count.second++;
                    }
                    std::fill(flags_temp.begin() + i, flags_temp.begin() + i + size, true);
                }
            }
        }
        flags_nested = flags_temp;
    }
}

struct counts_mt : public Worker {
    
    Texts texts;
    MapNgramsPair &counts_seq;
    const std::vector<unsigned int> &sizes;
    const unsigned int &id_mark;
    
    counts_mt(Texts texts_, MapNgramsPair &counts_seq_, const std::vector<unsigned int> &sizes_, 
              const unsigned int &id_mark_):
        texts(texts_), counts_seq(counts_seq_), sizes(sizes_), 
        id_mark(id_mark_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            counts(texts[h], counts_seq, sizes, id_mark);
        }
    }
};

void estimates(std::size_t i,
               VecNgrams &seqs,  // seqs without padding
               MapNgramsPair counts_seq,
               DoubleParams &dice,
               DoubleParams &pmi,
               DoubleParams &logratio,
               DoubleParams &chi2,
               DoubleParams &gensim,
               DoubleParams &lfmd,
               const String &method,
               const double smoothing) {
    
    std::size_t n = seqs[i].size(); //n=2:5, seqs
    if (n == 1) return; // ignore single words
    // output counts
    std::vector<double> counts_bit(std::pow(2, n), smoothing);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it) {
        if (it->first.size() != n) continue; // skip different lengths
        int bit;
        bit = match_bit2(seqs[i], it->first);
        counts_bit[bit] += it->second.first;
    }
    //counts_bit[std::pow(2, n)-1]  += cs_np[i];//  c(2^n-1) += number of itself  
    
    // // Dice coefficient
    // dice[i] = n * compute_dice(counts_bit);
    // 
    // // marginal counts: used in pmi, chi-sqaure, G2, gensim, LFMD
    // std::vector<double> mc(n, 0);  // the size of mc is n
    // for (int k = 1; k < std::pow(2, n); k++){
    //     int kk = k;
    //     for (int j = n-1; j >= 0; j--){
    //         int jj = std::pow(2, j);
    //         if (kk >= jj){
    //             mc[j] += counts_bit[k];
    //             kk -= jj;
    //         }
    //     }
    // }
    // 
    // double mc_product = 1;
    // for (std::size_t k = 0; k < n; k++){
    //     mc_product *= mc[k];
    // }
    // 
    // // calculate gensim score
    // // https://radimrehurek.com/gensim/models/phrases.html#gensim.models.phrases.Phrases
    // // gensim = (cnt(a, b) - min_count) * N / (cnt(a) * cnt(b))
    // gensim[i] = (counts_bit[std::pow(2, n) - 1] - count_min) * nseqs/mc_product;
    // 
    // //LFMD
    // //see http://www.lrec-conf.org/proceedings/lrec2002/pdf/128.pdf for details about LFMD
    // //LFMD = log2(P(w1,w2)^2/P(w1)P(w2)) + log2(P(w1,w2))
    // lfmd[i] = log2(counts_bit[std::pow(2, n) - 1] * counts_bit[std::pow(2, n) - 1] * pow(nseqs, n-2) / mc_product) 
    //     + log2(counts_bit[std::pow(2, n) - 1] / nseqs);
    // 
    // 
    // //******issue 803: count ngrams as 2x2 table**********//
    // std::vector<double> new_count(4, 0);
    // new_count[3] = counts_bit[std::pow(2, n) - 1]; // C(BA)C
    // new_count[1] = counts_bit[(std::pow(2, n-1) - 1)]; //~C(BA)
    // for (std::size_t k = 0; k < (std::pow(2, n-1) - 1); k++){
    //     new_count[2] = new_count[2] + counts_bit[k + std::pow(2, n-1)] - smoothing; //C~(AB)
    //     //if(i==1)Rcout<<"counts_bit["<<k + std::pow(2, n-1)<<"]="<<counts_bit[k+std::pow(2, n-1)]<<std::endl;
    //     new_count[0] = new_count[0] + counts_bit[k] - smoothing; //~C~(AB)
    //     
    // }
    // 
    // // adjust the impact of smoothing
    // new_count[2] += smoothing;
    // new_count[0] += smoothing;
    // //if(i==1)Rcout<<"new_count[0]="<<new_count[0]<<" "<<new_count[1]<<" "<<new_count[2]<<" "<<new_count[3]<<std::endl;
    // 
    // // marginal totals
    // std::vector<double> new_mc(2);
    // new_mc[0] = new_count[3] + new_count[1]; //?(BA)
    // new_mc[1] = new_count[3] + new_count[2]; //C(??)
    // //if(i==1)Rcout<<"new_mc[0]="<<new_mc[0]<<" "<<new_mc[1]<<std::endl;
    // //if(i==1) Rcout<<"total="<<nseqs<<"seqs"<<seqs[i][0]<<" "<<seqs[i][1]<<" "<<seqs[i][2]<<std::endl;
    // 
    // // expected totals
    // std::vector<double> new_ec(4);
    // new_ec[0] = (nseqs - new_mc[0])*(nseqs - new_mc[1])/nseqs;//
    // new_ec[1] = new_mc[0]*(nseqs - new_mc[1])/nseqs;
    // new_ec[2] = (nseqs - new_mc[0])* new_mc[1]/nseqs;
    // new_ec[3] = new_mc[0]* new_mc[1]/nseqs;
    // //if(i==1)Rcout<<"new_ec[0]="<<new_ec[0]<<" "<<new_ec[1]<<" "<<new_ec[2]<<" "<<new_ec[3]<<std::endl;
    // 
    // 
    // //pmi
    // pmi[i] = log(new_count[3]*nseqs/(new_mc[0]*new_mc[1]));
    // 
    // //logratio
    // logratio[i] = 0.0;
    // double epsilon = 0.000000001; // to offset zero cell counts
    // for (std::size_t k = 0; k < 4; k++){
    //     logratio[i] += new_count[k] * log(new_count[k]/new_ec[k] + epsilon);
    // }
    // logratio[i] *= 2;
    // 
    // //chi2
    // chi2[i] = 0.0;
    // for (std::size_t k = 0; k < 4; k++){
    //     chi2[i] += std::pow((new_count[k] - new_ec[k]), 2)/new_ec[k];
    // }
}

struct estimates_mt : public Worker{
    VecNgrams &seqs;
    MapNgramsPair &counts_seq;
    DoubleParams &dice;
    DoubleParams &pmi;
    DoubleParams &logratio;
    DoubleParams &chi2;
    DoubleParams &gensim;
    DoubleParams &lfmd;
    const String &method;
    const double smoothing;
    
    estimates_mt(VecNgrams &seqs_, MapNgramsPair &counts_seq_, DoubleParams &dice_,
                 DoubleParams &pmi_, DoubleParams &logratio_, DoubleParams &chi2_, DoubleParams &gensim_, DoubleParams &lfmd_, const String &method,
                 const double smoothing_):
        seqs(seqs_), counts_seq(counts_seq_), dice(dice_), pmi(pmi_), logratio(logratio_), chi2(chi2_),
        gensim(gensim_), lfmd(lfmd_), method(method), smoothing(smoothing_){}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t i = begin; i < end; i++) {
            estimates(i, seqs, counts_seq, dice, pmi, logratio, chi2, gensim, lfmd, method, smoothing);
        }
    }
};

void estimates_lambda(std::size_t i,
                      const VecNgrams &seqs,
                      const VecPair &seqs_all,
                      DoubleParams &sgma, 
                      DoubleParams &lmda,
                      const String &method,
                      const double smoothing) {
    
    std::size_t n = seqs[i].size();
    if (n == 1) return; // ignore single words
    
    std::vector<double> counts_bit(std::pow(2, n), smoothing);
    for (std::size_t j = 0; j < seqs_all.size(); j++) {
        if (seqs_all[j].first.size() != n) continue; // skip different lengths
        int bit = match_bit2(seqs[i], seqs_all[j].first);
        counts_bit[bit] += seqs_all[j].second;
    }
    
    //B-J algorithm    
    if (method == "lambda1"){
        sgma[i] = sigma_uni(counts_bit, n);
        lmda[i] = lambda_uni(counts_bit, n);
    } else {
        sgma[i] = sigma_all(counts_bit);
        lmda[i] = lambda_all(counts_bit, n);
    }
}

struct estimates_lambda_mt : public Worker{
    const VecNgrams &seqs;
    const VecPair &seq_all;
    DoubleParams &sgma;
    DoubleParams &lmda;
    const String &method;
    const double smoothing;
    
    estimates_lambda_mt(const VecNgrams &seqs_, const VecPair &seq_all_, DoubleParams &sgma_, DoubleParams &lmda_, 
                        const String &method, const double smoothing_) :
        seqs(seqs_), seq_all(seq_all_), sgma(sgma_), lmda(lmda_), 
        method(method), smoothing(smoothing_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t i = begin; i < end; i++) {
            estimates_lambda(i, seqs, seq_all, sgma, lmda, method, smoothing);
        }
    }
};


/* 
* This function estimates the strength of association between specified words 
* that appear in sequences. 
* @used sequences()
* @param texts_ tokens object
* @param count_min sequences appear less than this are ignored
* @param method method to estimate collocation association
* @param smoothing this number is added to collocation counts
* @param nested estimate parameters for nested collocations
*/
// [[Rcpp::export]]
DataFrame qatd_cpp_sequences(const List &texts_,
                             const CharacterVector &types_,
                             const IntegerVector &words_ignore_,
                             const unsigned int count_min,
                             const IntegerVector sizes_,
                             const String &method,
                             const double smoothing){
    
    Texts texts = as<Texts>(texts_);
    std::vector<unsigned int> sizes = as< std::vector<unsigned int> >(sizes_);
    std::sort(sizes.begin(), sizes.end(), std::greater<unsigned int>()); // sort in descending order
    std::vector<unsigned int> words_ignore = as< std::vector<unsigned int> >(words_ignore_);
    unsigned int id_mark = UINT_MAX; // use largest limit as filler
    
    SetUnigrams set_ignore;
    set_ignore.max_load_factor(GLOBAL_PATTERNS_MAX_LOAD_FACTOR);
    for (size_t g = 0; g < words_ignore.size(); g++) {
        set_ignore.insert(words_ignore[g]);
    }
    
    // replace ineligble tokens with special ID
#if QUANTEDA_USE_TBB
    replace_mt replace_mt(texts, set_ignore, id_mark);
    parallelFor(0, texts.size(), replace_mt, id_mark);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        texts[h] = replace(texts[h], set_ignore, id_mark);
    }
#endif
    
    MapNgramsPair counts_seq;
    counts_seq.max_load_factor(GLOBAL_PATTERNS_MAX_LOAD_FACTOR);
    
    //dev::Timer timer;
    //dev::start_timer("Count", timer);
#if QUANTEDA_USE_TBB
    counts_mt count_mt(texts, counts_seq, sizes, id_mark);
    parallelFor(0, texts.size(), count_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        counts(texts[h], counts_seq, sizes, id_mark);
    }
#endif
    //dev::stop_timer("Count", timer);
    
    VecNgrams seqs;
    VecPair seqs_all;
    IntParams counts, counts_nested, lengths;
    std::size_t len = counts_seq.size();
    seqs.reserve(len);
    seqs_all.reserve(len);
    counts.reserve(len);
    counts_nested.reserve(len);
    lengths.reserve(len);
    for (auto it = counts_seq.begin(); it != counts_seq.end(); ++it) {
        // conver to a vector for faster itteration
        seqs_all.push_back(std::make_pair(it->first, it->second.first)); 
        if (it->second.first < count_min) continue;
        // estimate only sequences without padding
        if (std::none_of(it->first.begin(), it->first.end(), [](unsigned int v){ return v == 0; })) {
            seqs.push_back(it->first);
            lengths.push_back(it->first.size());
            counts.push_back(it->second.first);
            counts_nested.push_back(it->second.second);
        }
    }
    
    std::size_t len_np = seqs.size();
    DoubleParams sgma(len_np), lmda(len_np);
    //DoubleParams dice(len_np), pmi(len_np), logratio(len_np), chi2(len_np), gensim(len_np), lfmd(len_np);
    
    //dev::start_timer("Estimate", timer);
#if QUANTEDA_USE_TBB
    estimates_lambda_mt estimate_mt(seqs, seqs_all, sgma, lmda, method, smoothing);
    parallelFor(0, seqs.size(), estimate_mt);
#else
    for (std::size_t i = 0; i < seqs.size(); i++) {
        estimates_lambda(i, seqs, seqs_all, sgma, lmda, method, smoothing);
    }
#endif
    //dev::stop_timer("Estimate", timer);
    
    
    // convert sequences from integer to character
    CharacterVector seqs_(seqs.size());
    for (std::size_t i = 0; i < seqs.size(); i++) {
        seqs_[i] = join_strings(seqs[i], types_, " ");
    }
    
    // Rcout << "collocation: " << seqs_.size() << "\n";
    // Rcout << "count: " << counts.size() << "\n";
    // Rcout << "count_nested: " << counts_nested.size() << "\n";
    // Rcout << "lmda: " << lmda.size() << "\n";
    // Rcout << "sgma: " << sgma.size() << "\n";
    
    DataFrame output_ = DataFrame::create(
        _["collocation"] = seqs_,
        _["count"] = as<IntegerVector>(wrap(counts)),
        _["count_nested"] = as<IntegerVector>(wrap(counts_nested)),
        _["length"] = as<NumericVector>(wrap(lengths)),
        _[method] = as<NumericVector>(wrap(lmda)),
        _["sigma"] = as<NumericVector>(wrap(sgma)),
        _["stringsAsFactors"] = false);
    
    return output_;
    
}


/***R
require(quanteda)
toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
id_mark <- unlist(quanteda:::regex2id("^\\p{P}+$", types, 'regex', FALSE), use.names = FALSE)
if (is.null(id_mark)) id_mark <- integer(0)
(out <- qatd_cpp_sequences(toks, types(toks), id_mark, 2, 2:3, "lambda", 0.5))
(out <- out[order(out$lambda, decreasing = TRUE),])
(out <- out[out$count != out$count_nested & out$length < max(out$length),])
txt <- "A gains capital B C capital gains A B capital C capital gains tax gains tax gains B gains C capital gains tax"
toks2 <- tokens(txt)
(out2 <- qatd_cpp_sequences(toks2, types(toks2), numeric(), 1, 3, "lambda", 0.0))
toks3 <- tokens('a b c . d e f')
(out3 <- qatd_cpp_sequences(toks3, types(toks3), 4, 1, 2:3, "lambda", 0.0))
*/
