
// includes from the plugin
#include <RcppArmadillo.h>
#include "quanteda.h"
using namespace quanteda;
using namespace RcppParallel;
using namespace Rcpp;
using namespace arma;

#if !defined(ARMA_64BIT_WORD)
#define ARMA_64BIT_WORD
#endif


// #if QUANTEDA_USE_TBB
// typedef std::tuple<unsigned int, unsigned int, double> Triplet;
// typedef tbb::concurrent_vector<Triplet> Triplets;
// #else
// typedef std::tuple<unsigned int, unsigned int, double> Triplet;
// typedef std::vector<Triplet> Triplets;
// #endif

//find the principle elements for the sparse residual matrix
void create_residual_ca(std::size_t row_num, const arma::sp_mat& objm, const arma::colvec &rsum, const arma::rowvec &csum,
                        const double residual_floor, const std::size_t K, Triplets &residual_tri)
{
    for (std::size_t k = 0; k < K; k++){
        double residual = (objm(row_num, k) - rsum(row_num) * csum(k)) / sqrt(rsum(row_num) * csum(k) );
        if (fabs(residual) > residual_floor) {
            Triplet mat_triplet = std::make_tuple(row_num, k, residual);
            residual_tri.push_back(mat_triplet);
        }
    }
}

//Create the residual matrix
struct Res : public Worker {
    const arma::sp_mat& objm;
    const arma::colvec &rsum;
    const arma::rowvec &csum;
    const double residual_floor;
    const std::size_t K;
    
    // output: residual[index0, index1, residual_value]
    Triplets &residual_tri;
    
    //constructor
    Res(const arma::sp_mat& objm, const arma::colvec &rsum, const arma::rowvec &csum, const double residual_floor, const std::size_t K, Triplets &residual_tri):
        objm(objm), rsum(rsum), csum(csum), residual_floor(residual_floor), K(K), residual_tri(residual_tri) {}
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            create_residual_ca(i, objm, rsum, csum, residual_floor, K, residual_tri);
        }
    }
};
// [[Rcpp::export]]

arma::sp_mat cacpp(const arma::sp_mat &objm, unsigned int threads, const double residual_floor){
    
    const std::size_t N = objm.n_rows;
    const std::size_t K = objm.n_cols;
    
    // Construct Chi-Sq Residuals	
    const arma::colvec rsum(sum(objm,1));
    const arma::rowvec csum(sum(objm,0));
    //double asum = accu(objm);	
    
    //create the residual matrix
    Triplets residual_tri;
    //residual_tri.reserve(N*K);
    if (threads == 1){
        for (std::size_t i = 0; i < N; i++) {
            create_residual_ca(i, objm, rsum, csum, residual_floor, K, residual_tri);
        }
    } else {
#if QUANTEDA_USE_TBB
        Res res(objm, rsum, csum, residual_floor, K, residual_tri);
        parallelFor(0, N, res);
        //Rcout<<"used TBB"<<std::endl;
#else
        for (std::size_t i = 0; i < N; i++) {
            create_residual_ca(i, objm, rsum, csum, residual_floor, K, residual_tri);
        }
#endif
    }
    
    // Convert to Rcpp objects
    std::size_t mat_size = residual_tri.size();
    arma::umat index_mat(2, mat_size, arma::fill::zeros);
    arma::vec w_values(mat_size, arma::fill::zeros);
    for (std::size_t k = 0; k < residual_tri.size(); k++) {
        index_mat(0,k) = std::get<0>(residual_tri[k]);
        index_mat(1,k) = std::get<1>(residual_tri[k]);
        w_values(k) = std::get<2>(residual_tri[k]);
    }
    
    // constract the sparse matrix
    arma::sp_mat spm(index_mat, w_values, N, K);
    return spm;
}

/***R
smoke<-matrix(c(4,2,3,2, 4,5,7,4,25,10,12,4,18,24,33,13,10,6,7,2), nrow = 5, ncol = 4, byrow = T)
threads = 7
residual_floor = 0.1
n = 195
P <- as.dfm(smoke)/n
cacpp(P, threads, residual_floor/sqrt(n))
*/
