#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "tbb/tbb.h"
#include "quanteda.h"
#include "tbb/concurrent_vector.h"

// [[Rcpp::plugins(cpp11)]]
using namespace RcppParallel;
using namespace Rcpp;
using namespace tbb;
using namespace quanteda;

#if RCPP_PARALLEL_USE_TBB
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef tbb::concurrent_vector<Triplet> Triplets;
#else
    typedef std::tuple<unsigned int, unsigned int, double> Triplet;
    typedef std::vector<Triplet> Triplets;
#endif   

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
                    
                    if (!tri & (text[i] != text[j]) ) { // add symmetric elements
                        Triplet mat_triplet = std::make_tuple(text[j] - 1, text[i] - 1, window_weights[j - i - 1]);
                        fcm_tri.push_back(mat_triplet);
                    }
                }else{
                    // because it is not ordered, for locations (x,y)(x>y) counts for location (y,x)
                    Triplet mat_triplet = std::make_tuple(text[j], text[i], window_weights[j - i - 1]);
                    fcm_tri.push_back(mat_triplet);
                    if (!tri & (text[i] != text[j]) ) {
                        Triplet mat_triplet = std::make_tuple(text[i], text[j], window_weights[j - i - 1]);
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

//Implement by calling TBB APIs
arma::sp_mat fcm_hash_cpp_mt(const Rcpp::List &texts_,
                             const int n_types,
                             const String &count,
                             const unsigned int window,
                             const NumericVector &weights,
                             const bool ordered,
                             const bool tri,
                             const unsigned int nvec){
    
    // triplets are constructed according to tri & ordered settings to be efficient
    if (count == "boolean"){
        // Currently, support for arma::sp_mat is preliminary, 
        // for exmaple Mat.find(Mat > 1) is not supported, so "booleanize" in matrix level is not applicable
        arma::sp_mat bFcm(n_types, n_types);
        for (int h = 0; h < texts_.size(); h++) {
            NumericVector text = texts_[h];
            unsigned int len = text.size();
            arma::sp_mat aFcm(n_types,n_types);
            for (unsigned int i = 0; i < text.size(); i++) {
                unsigned int id_i = text[i] -1 ;
                unsigned int j_int = i+1;
                unsigned int j_lim = std::min(i + window + 1, len);
                for(unsigned int j = j_int; j < j_lim; j++) {
                    unsigned int id_j = text[j]-1;
                    if (ordered){
                        // only include upper triangular element (diagonal inclusive) if tri = TRUE
                        if (tri){
                            if (id_i <= id_j) aFcm(id_i,id_j) = 1;
                        }else{
                            aFcm(id_i,id_j) = 1;
                        }
                    }else{
                        if (id_i <= id_j){
                            aFcm(id_i,id_j) = 1;
                            if (!tri) aFcm(id_j,id_i) = 1;
                        }else{
                            aFcm(id_j,id_i) = 1;
                            if (!tri) aFcm(id_i,id_j) = 1;
                        }
                    }
                }
            }
            bFcm += aFcm;
        }
        return bFcm;
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
        // declare the vector we will return
        Triplets fcm_tri;
        fcm_tri.reserve(nvec);
        
        // create the worker
        Fcmat_mt fcmat_mt(texts, window_weights, window, tri, ordered, fcm_tri);
        
        // call it with parallelFor
        parallelFor(0, texts.size(), fcmat_mt);
        
        //***********
        //for (std::size_t h = 0; h < texts.size(); h++) {
        //    fre_count(texts[h],window_weights, window, tri, ordered, fcm_tri);
        //}
        //*******
  
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

// implement serially
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

// [[Rcpp::export]]
arma::sp_mat fcm_hash_mt(Rcpp::List &texts,
                         const int n_types,
                         const String &count,
                         unsigned int window,
                         const NumericVector &weights,
                         const bool ordered,
                         const bool tri,
                         const unsigned int nvec){
    
    #if RCPP_PARALLEL_USE_TBB
    return fcm_hash_cpp_mt(texts, n_types, count, window, weights, ordered, tri, nvec);
    #else
    return fcm_hash_cpp(texts, n_types, count, window, weights, ordered, tri, nvec);
    #endif    
}

/***R

toks <- list(rep(1:10, 10), rep(5:15, 10))
types <- unique(unlist(toks))
fcm_hash_mt(toks, length(types), 'boolean', 3, c(1, 1, 1), TRUE, TRUE, 2)



*/
