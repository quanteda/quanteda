#include "armadillo.h"
#include "quanteda.h"
#include "dev.h"
using namespace quanteda;
using namespace arma;

rowvec nnz(const sp_mat& mt) {
    rowvec v(mt.n_cols, fill::zeros);
    if (mt.is_empty()) return(v);
    for (uword i = 0; i < mt.n_cols; i++) {
        v[i] = sum(colvec(mt.col(i)) != 0);
    }
    return(v);
}

rowvec stddev(const sp_mat& mt, const int norm_type) {
    rowvec v(mt.n_cols, fill::zeros);
    if (mt.is_empty()) return(v);
    for (uword i = 0; i < mt.n_cols; i++) {
        v[i] = stddev(colvec(mt.col(i)), norm_type);
    }
    return(v);
}

// std::vector<double> to_vector(const sp_mat& mt) {
//     return conv_to< std::vector<double> >::from(mat(mt));
// }

std::vector<double> to_vector(const rowvec& v) {
    return conv_to< std::vector<double> >::from(v);
}

// [[Rcpp::export]]
NumericVector qatd_cpp_sd(arma::sp_mat& mt) {
    std::vector<double> sds = to_vector(stddev(mt, 0));
    return wrap(sds);
}

// [[Rcpp::export]]
NumericVector qatd_cpp_nz(arma::sp_mat& mt) {
    std::vector<double> nzs = to_vector(nnz(mt));
    return wrap(nzs);
}


