#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "quanteda.h"
using namespace quanteda;
using std::pow;
using std::exp;
using std::sqrt;
using std::log;

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
                
                // Using iterator is slower
                // arma::sp_mat diff = abs(amat.col(j) - amat.col(i));
                // double  dist = 0;
                // for(arma::sp_mat::const_iterator it_i = diff.begin(); it_i != diff.end(); ++it_i){
                //     dist += *it_i;
                // }
                // rmat(j,i) = dist;
                
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

// Canberra distance: sum_i |x_i - y_i| / |x_i + y_i|
// The result is weighted by num_nonzeros_|x_i + y_i|/num_elements
struct CanberraDistance : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    CanberraDistance(const arma::sp_mat& amat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = i+1; j < nrow; j++) {
                arma::vec denom = abs(amat.col(j) + aa);
                arma::colvec bb = abs(amat.col(j) - aa)/denom;
                bb.replace(arma::datum::nan, 0);
                arma::vec non_zeros = nonzeros(denom);
                rmat(j,i) = (double)sum(bb) * (double)ncol/(double)non_zeros.n_elem;
            }
        }
    }
};

struct CanberraDistance2 : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    const arma::sp_mat& bmat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    CanberraDistance2(const arma::sp_mat& amat, const arma::sp_mat& bmat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol)
        : amat(amat), bmat(bmat), rmat(rmat), nrow(nrow), ncol(ncol) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = 0; j < nrow; j++) {
                arma::vec denom = abs(bmat.col(j) + aa); 
                arma::colvec bb = abs(bmat.col(j) - aa)/denom;
                bb.replace(arma::datum::nan, 0);
                arma::vec non_zeros = nonzeros(denom);
                rmat(i,j) = (double)(sum(bb) * ncol)/(double)non_zeros.n_elem;
                //Rcpp::Rcout<<j<<std::endl;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix qatd_CanberraPara_cpp(const arma::sp_mat& A, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::uword nrow = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow, nrow);
    
    // create the worker
    CanberraDistance canberraDistance(base_m, rmat, nrow, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow-1, canberraDistance);
    return rmat;
}

// [[Rcpp::export]]
NumericMatrix qatd_CanberraPara_cpp2(const arma::sp_mat& A, const arma::sp_mat& B, const int margin = 1) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::sp_mat second_m = (margin == 1)? B.t() : B;
    
    arma::uword nrow_base = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    arma::uword nrow_2nd = (margin == 1)?B.n_rows : B.n_cols;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow_base, nrow_2nd);
    
    // create the worker
    CanberraDistance2 canberraDistance(base_m, second_m, rmat, nrow_2nd, ncol);
    
    // call it with parallelFor
    parallelFor(0, nrow_base, canberraDistance);
    return rmat;
}

// Minkowski distance: (sum_i (x_i - y_i)^p)^(1/p)
struct MinkowskiDistance : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    double p;
    
    // initialize from Rcpp input and output matrixes 
    MinkowskiDistance(const arma::sp_mat& amat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol, double p)
        : amat(amat), rmat(rmat), nrow(nrow), ncol(ncol), p(p) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            //arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = i+1; j < nrow; j++) {
                //arma::colvec diff = pow(abs(amat.col(j) - aa), p);
                //rmat(j,i) = pow(sum(diff), 1/p);
                
                
                //use sp_mat iterator - it will skip the zero element
                //This method is faster than converting to dense vector and conducting pow() operation.
                arma::sp_mat diff = abs(amat.col(j) - amat.col(i));
                double  dist = 0;
                for(arma::sp_mat::const_iterator it_i = diff.begin(); it_i != diff.end(); ++it_i){
                    dist += pow(*it_i, p);
                }
                rmat(j,i) = pow(dist, 1/p);
            }
        }
    }
};

struct MinkowskiDistance2 : public Worker {
    
    // input matrix to read from
    const arma::sp_mat& amat;
    const arma::sp_mat& bmat;
    // output matrix to write to
    RMatrix<double> rmat;
    
    arma::uword nrow, ncol;
    double p;
    // initialize from Rcpp input and output matrixes 
    MinkowskiDistance2(const arma::sp_mat& amat, const arma::sp_mat& bmat, NumericMatrix& rmat, arma::uword nrow, arma::uword ncol, double p)
        : amat(amat), bmat(bmat), rmat(rmat), nrow(nrow), ncol(ncol), p(p) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec aa = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (std::size_t j = 0; j < nrow; j++) {
                arma::sp_mat diff = abs(amat.col(i) - bmat.col(j));
                double  dist = 0;
                for(arma::sp_mat::const_iterator it_i = diff.begin(); it_i != diff.end(); ++it_i){
                    dist += pow(*it_i, p);
                }
                rmat(i,j) = pow(dist, 1/p);
                
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix qatd_MinkowskiPara_cpp(const arma::sp_mat& A, const int margin = 1, const double p = 2) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::uword nrow = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow, nrow);
    
    // create the worker
    MinkowskiDistance minkowskiDistance(base_m, rmat, nrow, ncol, p);
    
    // call it with parallelFor
    parallelFor(0, nrow-1, minkowskiDistance);
    return rmat;
}

// [[Rcpp::export]]
NumericMatrix qatd_MinkowskiPara_cpp2(const arma::sp_mat& A, const arma::sp_mat& B, const int margin = 1, const double p = 2) {
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1)? A.t() : A;
    arma::sp_mat second_m = (margin == 1)? B.t() : B;
    
    arma::uword nrow_base = (margin == 1)?A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1)?A.n_cols : A.n_rows;
    arma::uword nrow_2nd = (margin == 1)?B.n_rows : B.n_cols;
    
    // allocate the matrix we will return
    NumericMatrix rmat(nrow_base, nrow_2nd);
    
    // create the worker
    MinkowskiDistance2 minkowskiDistance(base_m, second_m, rmat, nrow_2nd, ncol, p);
    
    // call it with parallelFor
    parallelFor(0, nrow_base, minkowskiDistance);
    return rmat;
}
