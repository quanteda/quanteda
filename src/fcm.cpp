#include "lib.h"
#include "dev.h"
using namespace quanteda;

struct hash_pair {
  size_t operator()(const std::pair<unsigned int, unsigned int> &p) const {
    
    unsigned int seed = 0;
    seed = p.first;
    seed += p.second << 16;
    return std::hash<unsigned int>()(seed);
  }
};

struct equal_pair {
  bool operator() (const std::pair<unsigned int, unsigned int> &p1,
                   const std::pair<unsigned int, unsigned int> &p2) const {
    return (p1.first == p2.first && p1.second == p2.second);
  }
};


typedef std::unordered_set<std::pair<unsigned int, unsigned int>, 
                           hash_pair, equal_pair> SetPair;

// find out if a pair of token exists in a document
bool exist(const unsigned int &x, const unsigned int &y,
           SetPair &set_pair){
    
    auto it = set_pair.insert(std::make_pair(x, y));
    return !it.second;
}

//count the co-occurance when count is set to "frequency" or "weighted"
void count_col(const Text &text,
               const std::vector<double> &weights,    
               const unsigned int &window,
               const bool &ordered,
               const bool &boolean,
               Triplets &fcm_tri) {
    
    SetPair set_pair;
    set_pair.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);
    
    unsigned int j_ini, j_lim;
    double weight;
    Triplet tripl;
    for (unsigned int i = 0; i < text.size(); i++) {
        if (text[i] == 0) continue; // skip padding
        j_ini = std::min((int)(i + 1), (int)text.size());
        j_lim = std::min((int)(i + window + 1), (int)text.size());
        for(unsigned int j = j_ini; j < j_lim; j++) {
            if (text[j] == 0) continue; // skip padding
            weight = weights[std::abs((int)j - (int)i) - 1];
            if (ordered) {
                if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                    //Rcout << i << " " << j << "\n";
                    tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight);
                    fcm_tri.push_back(tripl);
                }
            } else {
                if (text[i] < text[j]) {
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight);
                        fcm_tri.push_back(tripl);
                    }
                } else if (text[i] > text[j]){
                    if (!boolean || !exist(text[j] - 1, text[i] - 1, set_pair)) {
                        tripl = std::make_tuple(text[j] - 1, text[i] - 1, weight);
                        fcm_tri.push_back(tripl);
                    }
                } else {
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight * 2);
                        fcm_tri.push_back(tripl);
                    }
                } 
            }
        }
    }
}

struct count_col_mt : public Worker{
    const Texts &texts;
    const std::vector<double> &weights;    
    const unsigned int window;
    const bool ordered;
    const bool boolean;
    Triplets &fcm_tri; // output vector to write to, each Triplet contains i, j, x for the sparse matrix fcm.

    //initialization
    count_col_mt(const Texts &texts_, const std::vector<double> &weights_, const unsigned int window_, 
             const bool ordered_, const bool boolean_, Triplets &fcm_tri_): 
             texts(texts_),  weights(weights_),
             window(window_), ordered(ordered_), boolean(boolean_), fcm_tri(fcm_tri_) {}

    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t h = begin; h < end; h++) {
            count_col(texts[h], weights, window, ordered, boolean, fcm_tri);
        }
    }
};


// [[Rcpp::export]]
S4 qatd_cpp_fcm(const Rcpp::List &texts_,
                const int n_types,
                const NumericVector &weights_,
                const bool boolean,
                const bool ordered){
    
    // triplets are constructed according to tri & ordered settings to be efficient
    Texts texts = Rcpp::as<Texts>(texts_);
    std::vector<double> weights = Rcpp::as< std::vector<double> >(weights_);
    unsigned int window = weights.size();

    // declare the vector returned by parallelized procedure
    Triplets fcm_tri;

    //dev::Timer timer;
    //dev::start_timer("Count", timer);
    
#if QUANTEDA_USE_TBB
    count_col_mt count_col_mt(texts, weights, window, ordered, boolean, fcm_tri);
    parallelFor(0, texts.size(), count_col_mt);
#else        
    for (std::size_t h = 0; h < texts.size(); h++) {
        count_col(texts[h], weights, window, ordered, boolean, fcm_tri);
    }
#endif
    
    //dev::stop_timer("Count", timer);
    //dev::start_timer("Convert", timer);
    return to_matrix(fcm_tri, n_types, n_types, false);
}


/***R
RcppParallel::setThreadOptions(1)
toks <- list(rep(1:10, 10), rep(5:15, 10))
toks <- list(c(1, 4, 2, 3))
types <- unique(unlist(toks))
qatd_cpp_fcm(toks, max(types), c(1, 1), FALSE, TRUE)
qatd_cpp_fcm(toks, max(types), c(1, 1), FALSE, FALSE)

# for c(1, 2, 2, 3), window = 2
# 0 2 0
# 2 0 2
# 0 2 0


*/
