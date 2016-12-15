
// [[Rcpp::depends(RcppArmadillo,RcppParallel)]]

#include <RcppArmadillo.h>
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
using namespace RcppParallel;
using namespace Rcpp;

// Manhattan distance: sum_i |x_i - y_i|
struct ManhattanDistance : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    ManhattanDistance(const arma::sp_mat& amat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = i+1; j < nrow; j++) {
                arma::colvec bb = amat.col(j) - aa;
                rmat(j,i) = sum(abs(bb));
                //Rcpp::Rcout<<j<<std::endl;
            }
        }
    }
};

struct ManhattanDistance2 : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    const arma::sp_mat& bmat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;

    // initialize from Rcpp input and output matrixes 
    ManhattanDistance2(const arma::sp_mat& amat, const arma::sp_mat& bmat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), bmat(bmat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = 0; j < nrow; j++) {
                arma::colvec bb = bmat.col(j) - aa;
                rmat(i,j) = sum(abs(bb));
                //Rcpp::Rcout<<j<<std::endl;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix qatd_ManhattanPara_cpp(const arma::sp_mat& A, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::uword nrow = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow, nrow);
    
    // create the worker
    ManhattanDistance manhattanDistance(base_m, rmat, nrow, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow-1, manhattanDistance);
    return rmat;
}

// [[Rcpp::export]]
NumericMatrix qatd_ManhattanPara_cpp2(const arma::sp_mat& A, const arma::sp_mat& B, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::sp_mat second_m = (margin == 1)? B.t() : B;
    
    arma::uword nrow_base = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    arma::uword nrow_2nd = (margin == 1)?B.n_rows : B.n_cols;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow_base, nrow_2nd);
    
    // create the worker
    ManhattanDistance2 manhattanDistance(base_m, second_m, rmat, nrow_2nd, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow_base, manhattanDistance);
    return rmat;
}

// Maximum/Supremum distance: max_i |x_i - y_i|
struct MaximumDistance : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    MaximumDistance(const arma::sp_mat& amat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = i+1; j < nrow; j++) {
                arma::colvec bb = amat.col(j) - aa;
                rmat(j,i) = max(abs(bb));
                //Rcpp::Rcout<<j<<std::endl;
            }
        }
    }
};

struct MaximumDistance2 : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    const arma::sp_mat& bmat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    MaximumDistance2(const arma::sp_mat& amat, const arma::sp_mat& bmat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), bmat(bmat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = 0; j < nrow; j++) {
                arma::colvec bb = bmat.col(j) - aa;
                rmat(i,j) = max(abs(bb));
                //Rcpp::Rcout<<j<<std::endl;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix qatd_MaximumPara_cpp(const arma::sp_mat& A, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::uword nrow = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow, nrow);
    
    // create the worker
    MaximumDistance maximumDistance(base_m, rmat, nrow, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow-1, maximumDistance);
    return rmat;
}

// [[Rcpp::export]]
NumericMatrix qatd_MaximumPara_cpp2(const arma::sp_mat& A, const arma::sp_mat& B, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::sp_mat second_m = (margin == 1)? B.t() : B;
    
    arma::uword nrow_base = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    arma::uword nrow_2nd = (margin == 1)?B.n_rows : B.n_cols;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow_base, nrow_2nd);
    
    // create the worker
    MaximumDistance2 maximumDistance(base_m, second_m, rmat, nrow_2nd, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow_base, maximumDistance);
    return rmat;
}
