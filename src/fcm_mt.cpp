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

#if QUANTEDA_USE_TBB
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef tbb::concurrent_vector<Triplet> Triplets;
    typedef tbb::concurrent_unordered_multimap<std::pair<unsigned int, unsigned int>, unsigned int, hash_pair, equal_pair> docMaps;
#else
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef std::vector<Triplet> Triplets;
    typedef std::unordered_multimap<std::pair<unsigned int, unsigned int>, unsigned int, hash_pair, equal_pair> docMaps;
    
#endif   

/*
// hash function for pair<>
template <class T>
inline void hash_combine(unsigned int&seed, const T & v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

namespace std
{
    template<typename S, typename T> struct hash<pair<S, T>>
    {
        inline size_t operator()(const pair<S, T> & v) const
        {
            size_t seed = 0;
            ::hash_combine(seed, v.first);
            ::hash_combine(seed, v.second);
            return seed;
        }
    };
}
*/


//count the co-occurance when count is set to "frequency" or "weighted"
void fre_count(const Text &text,
               const std::vector<double> &window_weights,    
               const unsigned int &window,
               const bool &tri,
               const bool &ordered,
               Triplets &fcm_tri){
    
    const unsigned int len = text.size();
    
    for (unsigned int i = 0; i < text.size(); i++) {
        unsigned int j_ini = i + 1;
        unsigned int j_lim = std::min(i + window + 1, len);
        
        for(unsigned int j = j_ini; j < j_lim; j++) {
            if (ordered){
                if (!tri || ((text[i] <= text[j])&& tri) ){// only include upper triangular element (diagonal inclusive) if tri = TRUE
                    Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                    fcm_tri.push_back(mat_triplet);
                }
            }else{
                if (text[i] <= text[j]){
                    Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                    fcm_tri.push_back(mat_triplet);
                    
                    if (!tri && (text[i] != text[j]) ) { // add symmetric elements
                        Triplet mat_triplet = std::make_tuple(text[j] - 1, text[i] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                }else{
                    // because it is not ordered, for locations (x,y)(x>y) counts for location (y,x)
                    Triplet mat_triplet = std::make_tuple(text[j] - 1, text[i] - 1, window_weights[j - i - 1]);
                    fcm_tri.push_back(mat_triplet);
                    if (!tri && (text[i] != text[j]) ) {
                        Triplet mat_triplet = std::make_tuple(text[i] - 1, text[j] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                }
            } // end of if-ordered
        } // end of j-loop
    }// end of i-loop
}

struct Fcmat_mt : public Worker{
    // input list to read from
    const Texts &texts;
    const std::vector<double> &window_weights;    
    const unsigned int window;
    const bool tri;
    const bool ordered;
    Triplets &fcm_tri; // output vector to write to, each Triplet contains i, j, x for the sparse matrix fcm.

    //initialization
    Fcmat_mt(const Texts &texts_, const std::vector<double> &window_weights_, const unsigned int window_, 
             const bool tri_, const bool ordered_, Triplets &fcm_tri_): 
             texts(texts_),  window_weights(window_weights_),
             window(window_), tri(tri_), ordered(ordered_), fcm_tri(fcm_tri_) {}

    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t h = begin; h < end; h++) {
            fre_count(texts[h], window_weights, window, tri, ordered, fcm_tri);
        }
    }
};

// find out if a pair of token exists in a document
bool ifexist(const std::pair<unsigned int, unsigned int> &index_pair,
             docMaps &fcm_tri,
             const unsigned int &doc_index){
    auto range = fcm_tri.equal_range(index_pair);
    bool index_exist = false;
    for (auto it = range.first; it != range.second; ++it){
        if (it->second == doc_index) index_exist = true;
    }
    return index_exist;
}

//count the co-occurance when count = "boolean"
void boolean_count(const Text &text,
                   const unsigned int &window,
                   const bool &tri,
                   const bool &ordered,
                   docMaps &fcm_tri,
                   const unsigned int &doc_index){
    
    const unsigned int len = text.size();
    
    for (unsigned int i = 0; i < text.size(); i++) {
        unsigned int j_ini = i + 1;
        unsigned int j_lim = std::min(i + window + 1, len);
        
        for(unsigned int j = j_ini; j < j_lim; j++) {
            std::pair<unsigned int, unsigned int> ij_pair = std::make_pair(text[i] - 1, text[j] - 1);
            bool ij_exist = ifexist(ij_pair, fcm_tri, doc_index);
            
            std::pair<unsigned int, unsigned int> ji_pair = std::make_pair(text[j] - 1, text[i] - 1);
            bool ji_exist = ifexist(ji_pair, fcm_tri, doc_index);
            if (ordered && !ij_exist){
                if (!tri || ((text[i] <= text[j])&& tri) ){// only include upper triangular element (diagonal inclusive) if tri = TRUE
                    std::pair<unsigned int, unsigned int> index_pair = std::make_pair(text[i] - 1, text[j] - 1);
                    fcm_tri.insert(std::make_pair(index_pair, doc_index));
                }
            }else{
                if (text[i] <= text[j]){
                    if (!ij_exist){
                        std::pair<unsigned int, unsigned int> index_pair = std::make_pair(text[i] - 1, text[j] - 1);
                        fcm_tri.insert(std::make_pair(index_pair, doc_index));
                    }
                    
                    if (!ji_exist && !tri && (text[i] != text[j]) ) { // add symmetric elements
                        std::pair<unsigned int, unsigned int> index_pair = std::make_pair(text[j] - 1, text[i] - 1);
                        fcm_tri.insert(std::make_pair(index_pair, doc_index));
                    }
                }else{
                    // because it is not ordered, for locations (x,y)(x>y) counts for location (y,x)
                    if (!ji_exist){
                        std::pair<unsigned int, unsigned int> index_pair = std::make_pair(text[j] - 1, text[i] - 1);
                        fcm_tri.insert(std::make_pair(index_pair, doc_index));
                    }
                    if (!ij_exist && !tri && (text[i] != text[j]) ) {
                        std::pair<unsigned int, unsigned int> index_pair = std::make_pair(text[i] - 1, text[j] - 1);
                        fcm_tri.insert(std::make_pair(index_pair, doc_index));
                    }
                }
            } // end of if-ordered
        } // end of j-loop
    }// end of i-loop    
}

struct Boolmat_mt : public Worker{
    // input list to read from
    const Texts &texts;
    const unsigned int window;
    const bool tri;
    const bool ordered;
    docMaps &fcm_tri; // output vector to write to, each Triplet contains i, j, x for the sparse matrix fcm.
    
    //initialization
    Boolmat_mt(const Texts &texts_, const unsigned int window_, 
             const bool tri_, const bool ordered_, docMaps &fcm_tri_): 
        texts(texts_), window(window_), tri(tri_), ordered(ordered_), fcm_tri(fcm_tri_) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t h = begin; h < end; h++) {
            boolean_count(texts[h], window, tri, ordered, fcm_tri, h);
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
    if (count == "boolean"){
        Texts texts = Rcpp::as<Texts>(texts_);
        
        // declare the map returned by parallelized procedure
        docMaps fcm_tri;
#if QUANTEDA_USE_TBB
        Boolmat_mt boolmat_mt(texts, window, tri, ordered, fcm_tri);
        parallelFor(0, texts.size(), boolmat_mt);
#else        
        for (std::size_t h = 0; h < texts.size(); h++) {
            boolean_count(texts[h], window, tri, ordered, fcm_tri, h);
        }
#endif        
        // Convert to Rcpp objects
        std::size_t mat_size = fcm_tri.size();
        arma::umat index_mat(2, mat_size, arma::fill::zeros);
        arma::vec w_values(mat_size, arma::fill::zeros);
        unsigned int k = 0;
        std::pair<unsigned int, unsigned int> index_pair;
        for (auto it = fcm_tri.begin(); it != fcm_tri.end(); ++it ) {
            index_pair = it->first;
            index_mat(0,k) = index_pair.first;
            index_mat(1,k) = index_pair.second;
            w_values(k) = 1;
            k++;
        }
        
        // constract the sparse matrix
        arma::sp_mat a_fcm(TRUE, index_mat, w_values, n_types, n_types);
        return a_fcm;
    }else{
        Texts texts = Rcpp::as<Texts>(texts_);
        // define weights 
        std::vector<double> window_weights;
        window_weights.reserve(window);
        if (count == "frequency"){
            window_weights.assign(window, 1.0);
        }else if(count == "weighted"){ 
            if (weights.size() == 1){
                for (unsigned int i=1; i<= window; i++){
                    window_weights[i-1] = 1.0/i;
                }
            }else{
                window_weights = Rcpp::as< std::vector<double> >(weights);
            }
        }
        // declare the vector returned by parallelized procedure
        Triplets fcm_tri;
        fcm_tri.reserve(nvec);
        
#if QUANTEDA_USE_TBB
        Fcmat_mt fcmat_mt(texts, window_weights, window, tri, ordered, fcm_tri);
        parallelFor(0, texts.size(), fcmat_mt);
#else        
        for (std::size_t h = 0; h < texts.size(); h++) {
            fre_count(texts[h],window_weights, window, tri, ordered, fcm_tri);
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
        arma::sp_mat a_fcm(TRUE, index_mat, w_values, n_types, n_types);
        return a_fcm;
    }
}

/*
// implement serially
// [[Rcpp::export]]
arma::sp_mat fcm_hash_cpp(Rcpp::List &texts,
                          const int &n_types,
                          const String &count,
                          unsigned int &window,
                          const NumericVector &weights,
                          const bool &ordered,
                          const bool &tri,
                          const unsigned int &nvec) {
    
    // triplets are constructed according to tri & ordered settings to be efficient
    if (count == "boolean"){
        // Currently, support for arma::sp_mat is preliminary, 
        // for exmaple Mat.find(Mat > 1) is not supported, so "booleanize" in matrix level is not applicable
        arma::sp_mat bFcm(n_types, n_types);
        for (unsigned int h = 0; h < texts.size(); h++) {
            NumericVector text = texts[h];
            const unsigned int len = text.size();
            arma::sp_mat aFcm(n_types,n_types);
            for (unsigned int i = 0; i < text.size(); i++) {
                int id_i = text[i] -1 ;
                int j_int = i+1;
                unsigned int j_lim = std::min(i + window + 1, len);
                for(unsigned int j = j_int; j < j_lim; j++) {
                    int id_j = text[j]-1;
                    if (ordered){
                        // only include upper triangular element (diagonal inclusive) if tri = TRUE
                        if (tri){
                            if (id_i <= id_j) aFcm(id_i,id_j) = 1;
                        }else{
                            aFcm(id_i, id_j) = 1;
                        }
                    }else{
                        if (id_i <= id_j){
                            aFcm(id_i, id_j) = 1;
                            if (!tri) aFcm(id_j,id_i) = 1;
                        }else{
                            aFcm(id_j, id_i) = 1;
                            if (!tri) aFcm(id_i, id_j) = 1;
                        }
                    }
                }
            }
            bFcm += aFcm;
        }
        return bFcm;
    }else{
        arma::umat index_mat(2, nvec);
        arma::vec w_values(nvec);
        // define weights 
        NumericVector window_weights;
        if (count == "frequency"){
            window_weights = NumericVector(window, 1.0);
        }else if(count == "weighted"){ 
            if (weights.size() == 1){
                window_weights = NumericVector(window);
                for (unsigned int i = 1; i < window + 1 ; i++){
                    window_weights(i-1) = 1.0/i;
                }
            }else{
                window_weights = NumericVector(weights);
            }
        }
        
        int vFrom = 0;
        int vTo = 0; 
        for (unsigned int h = 0; h < texts.size(); h++) {
            arma::urowvec text = texts[h];
            text = text - 1;
            const unsigned int len = text.size();
            
            //pair up the numeric vector to locate the pair co_occurred.
            //for instance: text_vec[0:end-1] - text_vec[1:end] denotes all the pairs with the offset = 1;  
            for (unsigned int i = 0; i < window; i++) {
                if (!tri) {
                    unsigned int length = len - i -1;
                    vTo = vFrom + length -1;
                    //Rcout<<"vTo="<<vTo<<" length="<<length<<"\n";
                    index_mat.row(0).subvec(vFrom, vTo) = text.head(length); 
                    index_mat.row(1).subvec(vFrom, vTo) = text.tail(length);
                    w_values.subvec(vFrom, vTo).fill(window_weights[i]);
                    if (!ordered){  //if not ordered, a-b will be counted twice as a-b & b-a
                        vFrom = vTo + 1;
                        arma::uvec upper = find (text.tail(length) != text.head(length));
                        unsigned int upperLength = upper.size();
                        vTo = vFrom + upperLength -1;
                        
                        arma::urowvec mcol = text.tail(length);
                        index_mat.row(0).subvec(vFrom, vTo) = mcol.elem(upper).t();
                        
                        arma::urowvec mrow = text.head(length);
                        index_mat.row(1).subvec(vFrom, vTo) = mrow.elem(upper).t(); 
                        w_values.subvec(vFrom, vTo).fill(window_weights[i]);
                    }
                } else {
                    unsigned int length = len - i -1;
                    arma::uvec upper = find (text.tail(length) >= text.head(length));
                    unsigned int upperLength = upper.size();
                    vTo = vFrom + upperLength -1; 
                    //Rcout<<"vTo="<<vTo<<" length="<<length<<"\n";
                    
                    arma::urowvec mrow = text.head(length);
                    index_mat.row(0).subvec(vFrom, vTo) = mrow.elem(upper).t();
                    
                    arma::urowvec mcol = text.tail(length);
                    index_mat.row(1).subvec(vFrom, vTo) = mcol.elem(upper).t();
                    w_values.subvec(vFrom, vTo).fill(window_weights[i]);
                    if (!ordered){
                        vFrom = vTo + 1;
                        upper = find (text.tail(length) < text.head(length));
                        upperLength = upper.size();
                        vTo = vFrom + upperLength -1;
                        index_mat.row(1).subvec(vFrom, vTo) = mrow.elem(upper).t();
                        index_mat.row(0).subvec(vFrom, vTo) = mcol.elem(upper).t();
                        w_values.subvec(vFrom, vTo).fill(window_weights[i]);
                    }
                }
                vFrom = vTo + 1;
            }
        }
        arma::sp_mat a_fcm(TRUE, index_mat.cols(0, vTo), w_values.head(vFrom), n_types, n_types);
        return a_fcm;
    }
}
*/


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
