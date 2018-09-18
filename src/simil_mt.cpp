#include "armadillo.h"
#include "quanteda.h"
#include "dev.h"
using namespace quanteda;

double simil_cosine(arma::mat& col_i, arma::mat& col_j) {
    return(dot(col_i, col_j) / (sqrt(accu(pow(col_i, 2))) * sqrt(accu(pow(col_j, 2)))));
}

double simil_correlation(arma::mat& col_i, arma::mat& col_j) {
    return(as_scalar(cor(col_i, col_j)));
}

struct similarity : public Worker {
    
    const arma::sp_mat& mt; // input
    Triplets& simil_tri; // output
    const int method;
    const std::vector<unsigned int>& target;
    const double limit;
    arma::uword nrow, ncol;
    
    similarity(const arma::sp_mat& mt_, Triplets& simil_tri_,
              int method_, std::vector<unsigned int>& target_,
              double limit_, arma::uword nrow_, arma::uword ncol_) :
              mt(mt_), simil_tri(simil_tri_), method(method_), target(target_),
              limit(limit_), nrow(nrow_), ncol(ncol_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        // define local triplets
        std::vector<Triplet> simil_tri_temp;
        simil_tri_temp.reserve(target.size());
        
        double simil = 0;
        bool symm = target.size() == ncol;
        arma::mat col_i = arma::mat(nrow, 1);
        arma::mat col_j = arma::mat(nrow, 1);
        for (std::size_t i = begin; i < end; i++) {
            col_i = arma::mat(mt.col(i));
            //Rcout << col_i << "\n";
            std::size_t j;
            for (auto s : target) {
                j = s - 1;
                //Rcout << "i=" << i << " j=" << j << "\n";
                if (symm && j > i) continue;
                col_j = arma::mat(mt.col(j));
                switch (method){
                    case 1:
                        simil = simil_cosine(col_i, col_j);
                        break;
                    case 2:
                        simil = simil_correlation(col_i, col_j);
                        break;
                    default:
                        simil = 0;
                }
                //Rcout << " simil=" << simil << "\n";
                if (simil >= limit) {
                    simil_tri_temp.push_back(std::make_tuple(i, j, simil));
                }
            }
        }
        std::copy(simil_tri_temp.begin(), simil_tri_temp.end(), 
                  simil_tri.grow_by(simil_tri_temp.size()));
    }
};


// [[Rcpp::export]]
S4 qatd_cpp_similarity(const arma::sp_mat& mt, 
                       const int method,
                       const IntegerVector target_,
                       const double limit = -1.0) {
    
    arma::uword ncol = mt.n_cols;
    arma::uword nrow = mt.n_rows;
    std::vector<unsigned int> target = as< std::vector<unsigned int> >(target_);
    
    //dev::Timer timer;
    //dev::start_timer("Compute", timer);
    
    Triplets simil_tri;
    //if (limit == -1.0)
    //    simil_tri.reserve(ncol * target.size() * 0.5);
    similarity simil(mt, simil_tri, method, target, limit, nrow, ncol);
    parallelFor(0, ncol, simil);
    //dev::stop_timer("Compute", timer);
    
    //dev::start_timer("Convert", timer);
    std::size_t simil_size = simil_tri.size();
    IntegerVector dim_ = IntegerVector::create(ncol, ncol);
    IntegerVector i_(simil_size), j_(simil_size);
    NumericVector x_(simil_size);
    
    for (std::size_t k = 0; k < simil_tri.size(); k++) {
        i_[k] = std::get<0>(simil_tri[k]);
        j_[k] = std::get<1>(simil_tri[k]);
        x_[k] = std::get<2>(simil_tri[k]);
    }
    if (target.size() == ncol) {
        S4 simil_("dsTMatrix");
        simil_.slot("i") = i_;
        simil_.slot("j") = j_;
        simil_.slot("x") = x_;
        simil_.slot("Dim") = dim_;
        simil_.slot("uplo") = "L";
        //dev::stop_timer("Convert", timer);
        return simil_;
    } else {
        S4 simil_("dgTMatrix");
        simil_.slot("i") = i_;
        simil_.slot("j") = j_;
        simil_.slot("x") = x_;
        simil_.slot("Dim") = dim_;
        //dev::stop_timer("Convert", timer);
        return simil_;
    }
}
