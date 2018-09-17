#include "armadillo.h"
#include "quanteda.h"
using namespace quanteda;
using std::pow;
using std::exp;
using std::sqrt;
using std::log;


struct cosine_similarity : public Worker {
    
    const arma::sp_mat& amat; // input
    Triplets& simil_tri; // output
    const std::vector<unsigned int>& selection;
    double limit;
    arma::uword nrow, ncol;
    
    // initialize from Rcpp input and output matrixes 
    cosine_similarity(const arma::sp_mat& amat_, Triplets& simil_tri_, 
                      std::vector<unsigned int>& selection_,
                      double limit_, arma::uword nrow_, arma::uword ncol_) :
                      amat(amat_), simil_tri(simil_tri_), selection(selection_),
                      limit(limit_), nrow(nrow_), ncol(ncol_) {}
    
    // function call operator that work for the specified range (begin/end)
    void operator()(std::size_t begin, std::size_t end) {
        bool upper = selection.size() < nrow;
        for (std::size_t i = begin; i < end; i++) {
            arma::colvec col_i = amat.col(i) + arma::zeros<arma::colvec>(ncol);
            for (auto s : selection) {
                std::size_t j = s - 1;
                if (!upper && j >= i) continue;
                arma::colvec col_j = amat.col(j) + arma::zeros<arma::colvec>(ncol);
                double simil = dot(col_i, col_j) / (sqrt(accu(pow(col_i, 2))) * sqrt(accu(pow(col_j, 2))));
                //Rcout << "i=" << i << " j=" << j << " simil=" << simil << "\n";
                if (s >= limit) {
                    Triplet mat_triplet = std::make_tuple(i, j, simil);
                    simil_tri.push_back(mat_triplet);
                }
            }
        }
    }
};

// [[Rcpp::export]]
S4 qatd_cpp_cosine(const arma::sp_mat& A, 
                    const IntegerVector selection_,
                    const int margin = 1, 
                    double limit = 0) {
    
    //Transpose the dfm so that the distance can be calculated between columns.
    arma::sp_mat base_m = (margin == 1) ? A.t() : A;
    arma::uword nrow = (margin == 1) ? A.n_rows : A.n_cols;
    arma::uword ncol = (margin == 1) ? A.n_cols : A.n_rows;
    
    std::vector<unsigned int> selection = as< std::vector<unsigned int> >(selection_);
    
    Triplets simil_tri;
    cosine_similarity simil(base_m, simil_tri, selection, limit, nrow, ncol);
    parallelFor(0, nrow, simil);
    
    std::size_t simil_size = simil_tri.size();
    IntegerVector dim_ = IntegerVector::create(nrow, nrow);
    IntegerVector i_(simil_size), j_(simil_size);
    NumericVector x_(simil_size);
    
    for (std::size_t k = 0; k < simil_tri.size(); k++) {
        i_[k] = std::get<0>(simil_tri[k]);
        j_[k] = std::get<1>(simil_tri[k]);
        x_[k] = std::get<2>(simil_tri[k]);
    }
    
    S4 simil_("dgTMatrix");
    simil_.slot("i") = i_;
    simil_.slot("j") = j_;
    simil_.slot("x") = x_;
    simil_.slot("Dim") = dim_;
    
    //dev::stop_timer("Convert", timer);
    
    return simil_;
}

/***R
quanteda_options(threads = 8)
#mt <- dfm(c("a b c", "a b d", "b d e", "e"))
mt <- dfm(data_corpus_inaugural)

old <- as.matrix(textstat_simil(mt, method = "cosine"))
dim(old)
new <- as.matrix(textstat_simil2(mt, min_simil = 0, diag = TRUE))
dim(new)
old[1:6, 1:6]
new[1:6, 1:6]
microbenchmark::microbenchmark(
    textstat_simil2(mt, min_simil = 0.8, diag = TRUE),
    textstat_simil2(mt, min_simil = 0, diag = TRUE),
    textstat_simil(mt, method = "cosine")
)

microbenchmark::microbenchmark(
    textstat_simil2(mt, "feature", min_simil = 0.8, diag = TRUE),
    textstat_simil2(mt, "feature", min_simil = 0, diag = TRUE),
    textstat_simil(mt, "feature", method = "cosine")
)

*/

