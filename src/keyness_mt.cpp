#include "lib.h"
#include "dev.h"

using namespace quanteda;

using KeynessFun = std::function<double(double,double)>;

static const double epsilon = 0.000000001; // the same value as R code

inline double yates_correction(const double& a, const double& b, const double& c, const double& d) {
    double N = a + b + c + d;
    if (std::abs(a *  d - b * c) >= N / 2
         && ((a + b) * (a + c) / N < 5
          || (a + b) * (b + d) / N < 5
          || (a + c) * (c + d) / N < 5
          || (c + d) * (b + d) / N < 5
        )
    ) {
        return 0.5;
    } else {
        return 0.0;
    }
}

inline double williams_correction(const double& a, const double& b, const double& c, const double& d) {
    if(a * b * c * d == 0) return 1.0;
    double N = a + b + c + d;
    return 1.0 + (N / (a + b) + N / (c + d) - 1.0) * (N / (a + c) + N / (b + d) - 1.0) / (6.0 * N);
}

inline KeynessFun chisq_lambda(const arma::sp_mat& mt, const std::string& cor) {
    arma::colvec mrg(arma::sum(mt, 1));
    double tN = mrg(0);
    double rN = mrg(1);
    return [&](const double& a, const double& b) -> double {
        double c = tN - a, d = rN - b, N = a + b + c + d, E = (a + b) * (a + c) / N;
        double delta = (cor == "default" || cor == "yates") ? yates_correction(a, b, c, d) : 0.0;
        double q     = (cor == "williams") ? williams_correction(a, b, c, d) : 1.0;
        double num = N * std::pow(std::abs((a * d) - (b * c)) - (N * delta), 2.0);
        double den = (a + b) * (c + d) * (a + c) * (b + d) * (a > E ? 1.0 : -1.0) / q;
        return num / den;
    };
}

inline KeynessFun exact_lambda(const arma::sp_mat& mt, const std::string& cor) {
    Rcpp::warning("Exact keyness not supported yet");
    return [&](const double& t, const double& r) -> double {
        return -99.0;
    };
}

inline KeynessFun lr_lambda(const arma::sp_mat& mt, const std::string& cor) {
    arma::colvec mrg(arma::sum(mt, 1));
    const double tN = mrg(0);
    const double rN = mrg(1);
    return [&](const double& a, const double& b) -> double {
        double c = tN - a, d = rN - b, N = a + b + c + d, E = (a + b) * (a + c) / N;
        double aa = a, bb = b, cc = c , dd = d;
        if (cor == "default" || cor == "yates") {
            double delta = yates_correction(a, b, c, d);
            bool sign = a * d - b * c > 0;
            aa += sign ? -delta : delta;
            bb += sign ? delta : -delta;
            cc += sign ? delta : -delta;
            dd += sign ? -delta : delta;
        }
        
        double res = (2 * (
            aa * std::log(aa / ((aa + bb) * (aa + cc) / N) + epsilon) + 
            bb * std::log(bb / ((aa + bb) * (bb + dd) / N) + epsilon) +
            cc * std::log(cc / ((aa + cc) * (cc + dd) / N) + epsilon) +
            dd * std::log(dd / ((bb + dd) * (cc + dd) / N) + epsilon)
        )) * (a > E ? 1.0 : -1.0);
        
        if (cor == "williams") 
            res /= williams_correction(a, b, c, d);
        return res;
    };
}

inline KeynessFun pmi_lambda(const arma::sp_mat& mt, const std::string& cor, bool normal = false) {
    arma::colvec mrg(arma::sum(mt, 1));
    const double tN = mrg(0);
    const double rN = mrg(1);
    return [&](const double& a, const double& b) -> double {
        double c = tN - a, d = rN - b, N = a + b + c + d, E = (a + b) * (a + c) / N;
        double res = std::log(a / E + epsilon);
        if (normal)
            res *= (a > E ? 1.0 : -1.0) / (std::log(a / N) * -1.0);
        return res;
    };
}

inline KeynessFun get_keyness_func(const arma::sp_mat& mt, const std::string& msr, const std::string& cor) {
    if (msr == "chi2") {
        return chisq_lambda(mt, cor);
    } else if (msr == "exact") {
        return exact_lambda(mt, cor);
    } else if (msr == "lr") {
        return lr_lambda(mt, cor);
    } else if (msr == "pmi") {
        return pmi_lambda(mt, cor);
    } else {
        Rcpp::stop("Unknown keyness measure requested");
    }
}

struct KeynessWorker : public Worker {
    const arma::sp_mat& mt;
    KeynessFun fun;
    DoubleParams& v;
    
    KeynessWorker(
        const arma::sp_mat& mt_, const KeynessFun& fun_, DoubleParams& v_
    ) : mt(mt_), fun(fun_), v(v_) {}

    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            v[i] = fun(mt(0, i), mt(1, i));
        }
    }
};

//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qatd_cpp_keyness(
    arma::sp_mat& mt, const std::string measure, const std::string correction
) {
    DoubleParams stats(mt.n_cols);
    KeynessFun fun = get_keyness_func(mt, measure, correction);

    KeynessWorker keyness_worker(mt, fun, stats);
    RcppParallel::parallelFor(0, mt.n_cols, keyness_worker);

    return Rcpp::wrap(stats);
}
