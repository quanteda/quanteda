#include "armadillo.h"
#include "quanteda.h"

using namespace quanteda;

typedef Rcpp::NumericVector RVecD;

using VFunc = std::function<double(double,double)>;
using PFunc = std::function<double(double)>;

VFunc get_v_func( const arma::sp_mat& dfm, const std::string& msr, const std::string& cor ) {
    return []( double t, double r ){ return -1.0; };
}

PFunc get_p_func( const arma::sp_mat& dfm, const std::string& msr, const std::string& cor ) {
    return []( double v ){ return -1.0; };
}

struct KeynessWorker : public Worker {
    const arma::sp_mat& m_dfm;
    RVector<double> m_v;
    RVector<double> m_p;
    VFunc m_vf;
    PFunc m_pf;

    KeynessWorker(
        const arma::sp_mat& dfm, RVecD& v, RVecD& p, const VFunc& vfunc, const PFunc& pfunc
    ) : m_dfm( dfm ), m_vf( vfunc ), m_pf( pfunc ), m_v( v ), m_p( p ) {}

    void operator() ( std::size_t begin, std::size_t end ) {
        for( std::size_t i = begin; i < end; i++ ) {
            m_v[i] = m_vf( m_dfm( 0, i ), m_dfm( 1, i ) );
            m_p[i] = m_pf( m_v[i] );
        }
    }
};

//' @export
// [[Rcpp::export]]
Rcpp::DataFrame qatd_cpp_keyness(
    arma::sp_mat& dfm, const std::string measure, const std::string correction
) {
    RVecD v( dfm.n_cols );
    RVecD p( dfm.n_cols );
    VFunc vfunc = get_v_func( dfm, measure, correction );
    PFunc pfunc = get_p_func( dfm, measure, correction );

    KeynessWorker wrkr( dfm, v, p, vfunc, pfunc );
    RcppParallel::parallelFor( 0, dfm.n_cols, wrkr );

    return Rcpp::DataFrame::create( _["v"] = v, _["p"] = p );
}
