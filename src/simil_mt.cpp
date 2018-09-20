#include "armadillo.h"
#include "quanteda.h"
#include "dev.h"
using namespace quanteda;

double magni_cosine(arma::mat& col) {
    return(sqrt(accu(pow(col, 2))));
}

double magni_correlation(arma::mat& col) {
    return(as_scalar(stddev(col, 1)));
}

struct magnitude : public Worker {
    
    const arma::sp_mat& mt; // input
    DoubleParams& magni; // output
    const int method;
    
    magnitude(const arma::sp_mat& mt_, DoubleParams& magni_, int method_) :
              mt(mt_), magni(magni_), method(method_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        arma::uword ncol = mt.n_cols;
        arma::uword nrow = mt.n_rows;
        arma::mat col = arma::mat(nrow, 1);
        for (std::size_t i = begin; i < end; i++) {
            col = arma::mat(mt.col(i));
            //Rcout << "i=" << i << "\n";
            switch (method) {
                case 1:
                    magni[i] = magni_cosine(col);
                    break;
                case 2:
                    magni[i] = magni_correlation(col);
                    break;
                default:
                    magni[i] = 0;
            }
        }
    }
};

double simil_cosine(arma::mat& col_i, arma::mat& col_j, double magni_i, double magni_j) {
    return dot(col_i, col_j) / (magni_i * magni_j);
}

double simil_correlation(arma::mat& col_i, arma::mat& col_j, double magni_i, double magni_j) {
    return as_scalar(cov(col_i, col_j, 1)) / (magni_i * magni_j);
}

double simil_ejaccard(arma::mat& col_i, arma::mat& col_j, double weight = 1) {
    double e = accu(col_i % col_j);
    return e / (accu(pow(col_i, weight)) + accu(pow(col_j, weight)) - e);
}

double simil_edice(arma::mat& col_i, arma::mat& col_j, double weight = 1) {
    double e = accu(col_i % col_j);
    return (2 * e) / (accu(pow(col_i, weight)) + accu(pow(col_j, weight)));
}

double simil_hamann(arma::mat& col_i, arma::mat& col_j, double weight = 1) {
    double e = accu(col_i == col_j);
    double u = col_i.n_rows - e;
    return (e - (u * weight)) / (e + u);
}

double simil_faith(arma::mat& col_i, arma::mat& col_j) {
    double t = accu(col_i == 1 && col_j == 1);
    double f = accu(col_i == 0 && col_j == 0);
    double n = col_i.n_rows;
    return (t + (f / 2)) / n;
}

double dist_manhattan(arma::mat& col_i, arma::mat& col_j) {
    return accu(abs(col_i - col_j));
}

double dist_maximum(arma::mat& col_i, arma::mat& col_j) {
    return as_scalar(max(abs(col_i - col_j)));
}

double dist_canberra(arma::mat& col_i, arma::mat& col_j) {
    double n = col_i.n_rows;
    arma::mat m = abs(col_i) + abs(col_j);
    arma::mat b = abs(col_i - col_j);
    arma::mat d = b / m;
    d.replace(arma::datum::nan, 0);
    return accu(d) / (accu(m != 0) / n);
}

double dist_minkowski(arma::mat& col_i, arma::mat& col_j, double order = 1) {
    return(pow(accu(pow(abs(col_i) - abs(col_j), order)), 1 / order));
}

struct similarity : public Worker {
    
    const arma::sp_mat& mt; // input
    Triplets& simil_tri; // output
    const DoubleParams& magni;
    const int method;
    const std::vector<unsigned int>& target;
    const unsigned int rank;
    const double limit;
    const bool symm;
    const double weight;
    
    similarity(const arma::sp_mat& mt_, Triplets& simil_tri_, const DoubleParams& magni_,
               const int method_, const std::vector<unsigned int>& target_, 
               const unsigned int rank_, const double limit_, const bool symm_, const double weight_) :
               mt(mt_), simil_tri(simil_tri_), magni(magni_), 
               method(method_), target(target_), rank(rank_), limit(limit_), symm(symm_), weight(weight_) {}
    
    void operator()(std::size_t begin, std::size_t end) {
        
        arma::uword ncol = mt.n_cols;
        arma::uword nrow = mt.n_rows;
        
        std::vector<double> simil_temp;
        double simil = 0;
        
        arma::mat col_i = arma::mat(nrow, 1);
        arma::mat col_j = arma::mat(nrow, 1);
        std::size_t i;
        for (std::size_t h = begin; h < end; h++) {
            i = target[h] - 1;
            col_i = arma::mat(mt.col(i));
            //Rcout << col_i << "\n";
            simil_temp.reserve(ncol);
            for (std::size_t j = 0; j < ncol; j++) {
                //Rcout << "i=" << i << " j=" << j << "\n";
                if (symm && j > i) continue;
                col_j = arma::mat(mt.col(j));
                switch (method){
                    case 1:
                        simil = simil_cosine(col_i, col_j, magni[i], magni[j]);
                        break;
                    case 2:
                        simil = simil_correlation(col_i, col_j, magni[i], magni[j]);
                        break;
                    case 3:
                        simil = simil_ejaccard(col_i, col_j, weight);
                        break;
                    case 4:
                        simil = simil_edice(col_i, col_j, weight);
                        break;
                    case 5:
                        simil = simil_hamann(col_i, col_j, weight);
                        break;
                    case 6:
                        simil = simil_faith(col_i, col_j);
                        break;
                    default:
                        simil = 0;
                }
                //Rcout << "simil=" << simil << "\n";
                simil_temp.push_back(simil);
            }
            std::vector<double> simil_sort = simil_temp;
            double limit_temp = limit;
            if (ncol > rank) {
                std::nth_element(simil_sort.begin(), simil_sort.begin() + rank - 1, simil_sort.end(),
                                 std::greater<double>());
                if (limit_temp < simil_sort[rank - 1])
                    limit_temp = simil_sort[rank - 1];
            }
            //Rcout << "Limit: " << limit_temp << "\n";
            for (std::size_t k = 0; k < simil_temp.size(); k++) {
                if (simil_temp[k] != 0 && simil_temp[k] >= limit_temp) {
                    //Rcout << "Add: " <<  target[k] - 1 << " " << simil_temp[k] << "\n";
                    simil_tri.push_back(std::make_tuple(k, i, simil_temp[k]));
                }
            }
            simil_temp.clear();
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
    
    arma::uword ncol = mt.n_cols;
    arma::uword nrow = mt.n_rows;
    std::vector<unsigned int> target = as< std::vector<unsigned int> >(target_);
    if (rank < 1) rank = 1;
    bool symm = target.size() == ncol && rank == nrow;
    
    //dev::Timer timer;
    //dev::start_timer("Compute", timer);
    
    // compute magnitued for all columns
    DoubleParams mangi(ncol);
    if (method == 1 || method == 2) {
        magnitude magnitude(mt, mangi, method);
        parallelFor(0, ncol, magnitude);
    }
    
    // compute similarity for each pair
    Triplets simil_tri;
    //if (limit == -1.0)
    //    simil_tri.reserve(ncol * target.size() * 0.5);
    similarity similarity(mt, simil_tri, mangi, method, target, rank, limit, symm, weight);
    parallelFor(0, target.size(), similarity);
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
