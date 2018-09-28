#include "armadillo.h"
#include "quanteda.h"
#include "dev.h"
using namespace quanteda;
using namespace arma;

rowvec magni_cosine(const sp_mat& mt) {
    return rowvec(sqrt(mat(sum(mt % mt, 0))));
}

std::vector<double> to_vector(const sp_mat& mt) {
    return conv_to< std::vector<double> >::from(mat(mt));
}

double get_limit(std::vector<double> simil, const unsigned int rank, const double limit) {
    
    std::vector<double> simil_sort = simil;
    double limit_new = limit;
    if (simil.size() > rank) {
        std::nth_element(simil_sort.begin(), simil_sort.begin() + rank - 1, simil_sort.end(),
                         std::greater<double>());
        if (limit_new < simil_sort[rank - 1])
            limit_new = simil_sort[rank - 1];
    }
    return limit_new;
}

struct similarity_linear : public Worker {
    
    const arma::sp_mat& mt1; // input
    const arma::sp_mat& mt2; // input
    Triplets& simil_tri; // output
    const rowvec& magni;
    const int method;
    const std::vector<unsigned int>& target;
    const unsigned int rank;
    double limit;
    const bool symm;
    const double weight;
    
    similarity_linear(const sp_mat& mt1_, const sp_mat& mt2_, Triplets& simil_tri_, const rowvec& magni_,
               const int method_, const std::vector<unsigned int>& target_, 
               const unsigned int rank_, double limit_, const bool symm_, const double weight_) :
               mt1(mt1_), mt2(mt2_), simil_tri(simil_tri_), magni(magni_), 
               method(method_), target(target_), rank(rank_), limit(limit_), symm(symm_), weight(weight_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        uword ncol = mt1.n_cols;
        uword nrow = mt1.n_rows;
        
        std::vector<double> simils;
        uword i;
        for (std::size_t h = begin; h < end; h++) {
            i = target[h] - 1;
            //Rcout << "target: " << i << "\n";
            switch (method) {
            case 1:
                simils = to_vector(trans(mt2 * mt1.col(i)) / (magni * magni[i]));
                break;
            default:
                continue;
            } 
            limit = get_limit(simils, rank, limit);
            for (std::size_t k = 0; k < simils.size(); k++) {
                if (simils[k] != 0 && simils[k] >= limit) {
                    simil_tri.push_back(std::make_tuple(k, i, simils[k]));
                }
            }
            simils.clear();
        }
    }
};

// [[Rcpp::export]]
S4 qatd_cpp_similarity_linear(const arma::sp_mat& mt, 
                       const int method,
                       const IntegerVector target_,
                       unsigned int rank,
                       double limit = -1.0,
                       const double weight = 1.0,
                       bool condition_ = false) {
    
    const arma::sp_mat& mt1 = mt;
    const arma::sp_mat& mt2 = trans(mt);
    arma::uword ncol = mt.n_cols;
    arma::uword nrow = mt.n_rows;
    std::vector<unsigned int> target = as< std::vector<unsigned int> >(target_);
    if (rank < 1) rank = 1;
    bool symm = target.size() == ncol && rank == nrow;
    
    //dev::Timer timer;
    rowvec mangi = magni_cosine(mt);
    //DoubleParams mangi = mangi_temp;
    //Rcout << mangi << "\n";
    //dev::start_timer("Compute magnitude", timer);
    
    //dev::stop_timer("Compute magnitude", timer);
    
    //dev::start_timer("Compute similarity", timer);
    // compute similarity for each pair
    Triplets simil_tri;
    //if (limit == -1.0)
    //    simil_tri.reserve(ncol * target.size() * 0.5);
    similarity_linear similarity_linear(mt1, mt2, simil_tri, mangi, method, target, 
                                        rank, limit, symm, weight);
    parallelFor(0, target.size(), similarity_linear);
    //dev::stop_timer("Compute similarity", timer);
    
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
    if (symm) {
        S4 simil_("dsTMatrix");
        simil_.slot("i") = i_;
        simil_.slot("j") = j_;
        simil_.slot("x") = x_;
        simil_.slot("Dim") = dim_;
        simil_.slot("uplo") = "U";
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
