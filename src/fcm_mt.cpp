#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "quanteda.h"
using namespace quanteda;

struct hash_pair {
  size_t operator()(const pair<unsigned int, unsigned int> &p) const {
    
    // Old potentially broken
    // unsigned int hash = 0;
    // hash ^= std::hash<unsigned int>()(p.first) + 0x9e3779b9;
    // hash ^= std::hash<unsigned int>()(p.second) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
    // return hash;
    
    unsigned int seed = 0;
    seed = p.first;
    seed += p.second << 16;
    return std::hash<unsigned int>()(seed);
  }
};

struct equal_pair {
  bool operator() (const pair<unsigned int, unsigned int> &p1, 
                   const pair<unsigned int, unsigned int> &p2) const { 
    return (p1.first == p2.first && p1.second == p2.second);
  }
};

typedef std::unordered_set<std::pair<unsigned int, unsigned int>, hash_pair, equal_pair> SetPair;

// find out if a pair of token exists in a document
bool exist(const unsigned int &x, const unsigned int &y,
           SetPair &set_pair){
    
    auto it = set_pair.insert(std::make_pair(x, y));
    //Rcout << x << "-" << y << " " << it.second << "\n";
    return !it.second;
}

//count the co-occurance when count is set to "frequency" or "weighted"
void count_col(const Text &text,
           const std::vector<double> &window_weights,    
           const unsigned int &window,
           const bool &tri,
           const bool &ordered,
           const bool &boolean,
           Triplets &fcm_tri){
    
    const unsigned int len = text.size();
    SetPair set_pair;
    
    for (unsigned int i = 0; i < text.size(); i++) {
        unsigned int j_ini = i + 1;
        unsigned int j_lim = std::min(i + window + 1, len);
        
        for(unsigned int j = j_ini; j < j_lim; j++) {
            if (ordered){
                if (!tri || ((text[i] <= text[j])&& tri) ){// only include upper triangular element (diagonal inclusive) if tri = TRUE
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                }
            }else{
                if (text[i] <= text[j]){
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                    
                    if (!tri && (text[i] != text[j]) ) { // add symmetric elements
                        if (!boolean || !exist(text[j] - 1, text[i] - 1, set_pair)) {
                            Triplet mat_triplet = std::make_tuple(text[j] - 1, text[i] - 1, window_weights[j - i - 1]);
                            fcm_tri.push_back(mat_triplet);
                        }
                    }
                }else{
                    // because it is not ordered, for locations (x,y)(x>y) counts for location (y,x)
                    if (!boolean || !exist(text[j] - 1, text[i] - 1, set_pair)) {
                        Triplet mat_triplet = std::make_tuple(text[j] - 1, text[i] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                    
                    if (!tri && (text[i] != text[j]) ) {
                        if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                            Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                            fcm_tri.push_back(mat_triplet);
                        }
                    }
                }
            } // end of if-ordered
        } // end of j-loop
    }// end of i-loop
}

struct count_col_mt : public Worker{
    // input list to read from
    const Texts &texts;
    const std::vector<double> &window_weights;    
    const unsigned int window;
    const bool tri;
    const bool ordered;
    const bool boolean;
    Triplets &fcm_tri; // output vector to write to, each Triplet contains i, j, x for the sparse matrix fcm.

    //initialization
    count_col_mt(const Texts &texts_, const std::vector<double> &window_weights_, const unsigned int window_, 
             const bool tri_, const bool ordered_, const bool boolean_, Triplets &fcm_tri_): 
             texts(texts_),  window_weights(window_weights_),
             window(window_), tri(tri_), ordered(ordered_), boolean(boolean_), fcm_tri(fcm_tri_) {}

    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t h = begin; h < end; h++) {
            count_col(texts[h], window_weights, window, tri, ordered, boolean, fcm_tri);
        }
    }
};



// [[Rcpp::export]]
arma::sp_mat qatd_cpp_fcm(const Rcpp::List &texts_,
                             const int n_types,
                             const String &count,
                             const unsigned int window,
                             const NumericVector &weights,
                             const bool ordered,
                             const bool tri,
                             const unsigned int nvec){
    
    // triplets are constructed according to tri & ordered settings to be efficient
    Texts texts = Rcpp::as<Texts>(texts_);
    // define weights 
    std::vector<double> window_weights(window, 1.0);
    bool boolean = false;
    if (count == "boolean") {
        boolean = true;
    }else if(count == "weighted"){ 
        if (weights.size() == 1) {
            for (unsigned int i = 1; i <= window; i++){
                window_weights[i-1] = 1.0 / i;
            }
        }else{
            window_weights = Rcpp::as< std::vector<double> >(weights);
        }
    }
    // declare the vector returned by parallelized procedure
    Triplets fcm_tri;
    fcm_tri.reserve(nvec);
    
#if QUANTEDA_USE_TBB
    count_col_mt count_col_mt(texts, window_weights, window, tri, ordered, boolean, fcm_tri);
    parallelFor(0, texts.size(), count_col_mt);
#else        
    for (std::size_t h = 0; h < texts.size(); h++) {
        count_col(texts[h], window_weights, window, tri, ordered, boolean, fcm_tri);
    }
#endif
    
    // Convert to Rcpp objects
    std::size_t mat_size = fcm_tri.size();
    arma::umat index_mat(2, mat_size, arma::fill::zeros);
    arma::vec w_values(mat_size, arma::fill::zeros);
    for (std::size_t k = 0; k < fcm_tri.size(); k++) {
        index_mat(0,k) = std::get<0>(fcm_tri[k]);
        index_mat(1,k) = std::get<1>(fcm_tri[k]);
        w_values(k) = std::get<2>(fcm_tri[k]);
    }
    
    // constract the sparse matrix
    arma::sp_mat fcm(TRUE, index_mat, w_values, n_types, n_types);
    return fcm;
}


/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
types <- unique(unlist(toks))
window <- 2
n <- length(unlist(toks)) * window * 2
qatd_cpp_fcm(toks, length(types), 'weighted', window, c(1, 0.5, 0.1), TRUE, TRUE, n)

qatd_cpp_fcm(toks, length(types), 'boolean', 2, 1, TRUE, TRUE, 840)
microbenchmark::microbenchmark(
  boolean=qatd_cpp_fcm(toks, length(types), 'boolean', 2, 1, TRUE, TRUE, 840),
  weighted=qatd_cpp_fcm(toks, length(types), 'weighted', 2, c(1, 0.5, 0.1), TRUE, TRUE, 840)
)

*/
