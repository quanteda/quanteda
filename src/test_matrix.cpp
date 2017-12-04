#include "armadillo.h"
#include "quanteda.h"
//#include <RcppEigen.h>
using namespace quanteda;
using namespace arma;

// [[Rcpp::export]]
void qutd_cpp_matrix_test(const arma::sp_mat &dfm){

    Rcout << "Converting\n";
    const std::size_t N = dfm.n_rows;
    const std::size_t K = dfm.n_cols;
    Rcout << "Size " << N << " x " << K << "\n";
    
}

// typedef Eigen::SparseMatrix<double> SpMat;
// 
// // [[Rcpp::export]]
// void qutd_cpp_matrix_test2(SEXP dfm_){
//     
//     Rcout << "Converting\n";
//     SpMat dfm = Rcpp::as<SpMat>(dfm_);
//     const std::size_t N = dfm.n_rows;
//     const std::size_t K = dfm.n_cols;
// 
// }

/***R
require(quanteda)
small_dfm <- dfm(data_char_ukimmig2010)
qutd_cpp_matrix_test(small_dfm)

load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
large_dfm <- dfm(data_tokens_guardian)
qutd_cpp_matrix_test(large_dfm)
*/