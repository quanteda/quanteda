#include "armadillo.h"
#include "quanteda.h"
#include "dev.h"
using namespace quanteda;
using namespace arma;

std::vector<double> to_vector(const sp_mat& mt) {
    return conv_to< std::vector<double> >::from(mat(mt));
}

std::vector<double> to_vector(const rowvec& v) {
    return conv_to< std::vector<double> >::from(v);
}

double get_limit(std::vector<double> simils, const unsigned int rank, double limit) {
    
    if (simils.size() > rank) {
        std::nth_element(simils.begin(), simils.begin() + rank - 1, simils.end(),
                         std::greater<double>());
        if (limit < simils[rank - 1])
            limit = simils[rank - 1];
    }
    return limit;
}

rowvec stddev(const sp_mat& mt, const int norm_type) {
    rowvec v(mt.n_cols);
    for (uword i = 0; i < mt.n_cols; i++) {
        v[i] = stddev(colvec(mt.col(i)), norm_type);
    }
    return(v);
}

rowvec mean(const sp_mat& mt) {
    rowvec v(mt.n_cols);
    for (uword i = 0; i < mt.n_cols; i++) {
        v[i] = mean(colvec(mt.col(i)));
    }
    return(v);
}

struct similarity_linear : public Worker {
    
    const arma::sp_mat& mt1; // input
    const arma::sp_mat& mt2; // input
    Triplets& simil_tri; // output
    const rowvec& square;
    const rowvec& center;
    const int method;
    const std::vector<unsigned int>& target;
    const unsigned int rank;
    double limit;
    const bool symm;
    
    similarity_linear(const sp_mat& mt1_, const sp_mat& mt2_, Triplets& simil_tri_, 
                      const rowvec& square_, const rowvec& center_,
                      const int method_, const std::vector<unsigned int>& target_, 
                      const unsigned int rank_, double limit_, const bool symm_) :
        mt1(mt1_), mt2(mt2_), simil_tri(simil_tri_), square(square_), center(center_), 
        method(method_), target(target_), rank(rank_), limit(limit_), symm(symm_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        uword ncol = mt1.n_cols;
        uword nrow = mt1.n_rows;
        
        std::vector<double> simils;
        uword i;
        for (std::size_t h = begin; h < end; h++) {
            i = target[h] - 1;
            if (method == 1) {
                simils = to_vector(trans(mt2 * mt1.col(i)) / (square * square[i]));
            } else {
                rowvec v1 = rowvec(trans(mt2 * mt1.col(i)));
                rowvec v2 = center * center[i] * nrow;
                simils = to_vector(((v1 - v2) / nrow) / (square * square[i]));
            }
            limit = get_limit(simils, rank, limit);
            for (std::size_t k = 0; k < simils.size(); k++) {
                if (symm && k > i) continue;
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
                              const IntegerVector targets_,
                              unsigned int rank,
                              double limit = -1.0,
                              bool condition = false) {
    
    const sp_mat& mt1 = mt;
    const sp_mat& mt2 = trans(mt);
    uword ncol = mt1.n_cols;
    uword nrow = mt1.n_rows;
    std::vector<unsigned int> targets = as< std::vector<unsigned int> >(targets_);
    if (rank < 1) rank = 1;
    bool symm = targets.size() == ncol && rank == ncol;
    
    //dev::Timer timer;
    //dev::start_timer("Compute magnitude", timer);
    rowvec square(ncol), center(ncol);
    if (method == 1) {
        square = rowvec(sqrt(mat(sum(mt1 % mt1, 0))));
    } else {
        square = stddev(mt1, 1);
        center = mean(mt1);
    }
    
    //dev::stop_timer("Compute magnitude", timer);
    //dev::start_timer("Compute similarity", timer);
    Triplets simil_tri;
    similarity_linear similarity_linear(mt1, mt2, simil_tri, square, center, method, targets, 
                                        rank, limit, symm);
    parallelFor(0, targets.size(), similarity_linear);
    //dev::stop_timer("Compute similarity", timer);
    
    return to_matrix(simil_tri, ncol, ncol, symm);
}

double simil_ejaccard(colvec& col_i, colvec& col_j, double weight = 1) {
    double e = accu(col_i % col_j);
    return e / (accu(pow(col_i, weight)) + accu(pow(col_j, weight)) - e);
}

double simil_edice(colvec& col_i, colvec& col_j, double weight = 1) {
    double e = accu(col_i % col_j);
    return (2 * e) / (accu(pow(col_i, weight)) + accu(pow(col_j, weight)));
}

double simil_hamann(colvec& col_i, colvec& col_j, double weight = 1) {
    double e = accu(col_i == col_j);
    double u = col_i.n_rows - e;
    return (e - (u * weight)) / (e + u);
}

double simil_faith(colvec& col_i, colvec& col_j) {
    double t = accu(col_i == 1 && col_j == 1);
    double f = accu(col_i == 0 && col_j == 0);
    double n = col_i.n_rows;
    return (t + (f / 2)) / n;
}

double dist_manhattan(colvec& col_i, colvec& col_j) {
    return accu(abs(col_i - col_j));
}

double dist_maximum(colvec& col_i, colvec& col_j) {
    return as_scalar(max(abs(col_i - col_j)));
}

double dist_canberra(colvec& col_i, colvec& col_j) {
    double n = col_i.n_rows;
    colvec m = abs(col_i) + abs(col_j);
    colvec b = abs(col_i - col_j);
    colvec d = b / m;
    d.replace(datum::nan, 0);
    return accu(d) / (accu(m != 0) / n);
}

double dist_minkowski(colvec& col_i, colvec& col_j, double order = 1) {
    return pow(accu(pow(abs(col_i) - abs(col_j), order)), 1 / order);
}


struct similarity : public Worker {
    
    const sp_mat& mt; // input
    Triplets& simil_tri; // output
    const int method;
    const std::vector<unsigned int>& targets;
    const unsigned int rank;
    double limit;
    const bool symm;
    const double weight;
    
    similarity(const sp_mat& mt_, Triplets& simil_tri_, 
               const int method_, const std::vector<unsigned int>& targets_, 
               const unsigned int rank_, double limit_, const bool symm_, const double weight_) :
               mt(mt_), simil_tri(simil_tri_), 
               method(method_), targets(targets_), rank(rank_), limit(limit_), symm(symm_), weight(weight_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        arma::uword ncol = mt.n_cols;
        arma::uword nrow = mt.n_rows;
        
        double simil = 0;
        std::vector<double> simils;
        colvec col_i(nrow);
        colvec col_j(nrow);
        uword i;
        for (std::size_t h = begin; h < end; h++) {
            i = targets[h] - 1;
            col_i = mt.col(i);
            simils.reserve(ncol);
            
            for (uword j = 0; j < ncol; j++) {
                //Rcout << "i=" << i << " j=" << j << "\n";
                if (symm && j > i) continue;
                col_j = mt.col(j);
                switch (method) {
                case 1:
                    //simil = simil_ejaccard(col_i, col_j, weight);
                    break;
                case 2:
                    //simil = simil_edice(col_i, col_j, weight);
                    break;
                case 3:
                    //simil = simil_hamann(col_i, col_j, weight);
                    break;
                case 4:
                    //simil = simil_faith(col_i, col_j);
                    break;
                }
                //Rcout << "simil=" << simil << "\n";
                simils.push_back(simil);
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
S4 qatd_cpp_similarity(const arma::sp_mat& mt, 
                       const int method,
                       const IntegerVector target_,
                       unsigned int rank,
                       double limit = -1.0,
                       const double weight = 1.0) {
    
    uword ncol = mt.n_cols;
    uword nrow = mt.n_rows;
    std::vector<unsigned int> target = as< std::vector<unsigned int> >(target_);
    if (rank < 1) rank = 1;
    bool symm = target.size() == ncol && rank == ncol;
    
    //dev::Timer timer;
    //dev::start_timer("Compute similarity", timer);
    Triplets simil_tri;
    similarity similarity(mt, simil_tri, method, target, rank, limit, symm, weight);
    parallelFor(0, target.size(), similarity);
    //dev::stop_timer("Compute similarity", timer);
    
    return to_matrix(simil_tri, ncol, ncol, symm); 

}
