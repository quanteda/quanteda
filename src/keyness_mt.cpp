#include "armadillo.h"
#include "quanteda.h"

using namespace quanteda;

typedef Rcpp::NumericVector RVecD;

using VFunc = std::function<double(double,double)>;
using PFunc = std::function<double(double)>;

inline double yates_correction( const double& a, const double& b, const double& c, const double& d ) {
    double N = a + b + c + d;
    if( std::abs( a *  d - b * c ) >= N / 2
        && ( ( a + b ) * ( a + c ) / N < 5
          || ( a + b ) * ( b + d ) / N < 5
          || ( a + c ) * ( c + d ) / N < 5
          || ( c + d ) * ( b + d ) / N < 5
        )
    ) return N * 0.5;
    return 0.0;
}

inline double williams_correction( const double& a, const double& b, const double& c, const double& d ) {
    if( a * b * c * d == 0 ) return 1.0;
    double N = a + b + c + d;
    return 1.0 + ( N / ( a + b ) + N / ( c + d ) - 1.0 ) * ( N / ( a + c ) + N / ( b + d ) - 1.0 ) / ( 6.0 * N );
}

inline VFunc chisq_lambda( const arma::sp_mat& dfm, const std::string& cor ) {
    arma::colvec mrg( arma::sum( dfm, 1 ) );
    double tN = mrg( 0 );
    double rN = mrg( 1 );
    return [&]( const double& a, const double& b ) -> double {
        double c = tN - a, d = rN - b, N = a + b + c + d, E = ( a + b ) * ( a + c ) / N;
        double delta = ( cor == "default" || cor == "yates" ) ? yates_correction( a, b, c, d ) : 0.0;
        double q     = ( cor == "williams" ) ? williams_correction( a, b, c, d ) : 1.0;
        double num = N * std::pow( std::abs( ( a * d ) - ( b * c ) ) - delta, 2.0 );
        double den = ( a + b ) * ( c + d ) * ( a + c ) * ( b + d ) * ( a > E ? 1.0 : -1.0 ) / q;
        return num / den;
    };
}

inline VFunc exact_lambda( const arma::sp_mat& dfm, const std::string& cor ) {
    return [&]( const double& t, const double& r ) -> double {
        return -2.0;
    };
}

inline VFunc lr_lambda( const arma::sp_mat& dfm, const std::string& cor ) {
    return [&]( const double& t, const double& r ) -> double {
        return -3.0;
    };
}

inline VFunc pmi_lambda( const arma::sp_mat& dfm, const std::string& cor ) {
    return [&]( const double& t, const double& r ) -> double {
        return -4.0;
    };
}

inline VFunc get_v_func( const arma::sp_mat& dfm, const std::string& msr, const std::string& cor ) {
    if( msr == "chi2" ) {
        return chisq_lambda( dfm, cor );
    } else if( msr == "exact") {
        return exact_lambda( dfm, cor );
    } else if( msr == "lr" ) {
        return lr_lambda( dfm, cor );
    } else if( msr == "pmi" ) {
        return pmi_lambda( dfm, cor );
    } else {
        Rcpp:stop( "Unknown keyness measure requested" );
    }
}

PFunc get_p_func( const arma::sp_mat& dfm, const std::string& msr, const std::string& cor ) {
    return []( double v ){ return -1.0; };
}

struct KeynessWorker : public Worker {
    const arma::sp_mat& m_dfm;
    RVecD m_v;
    RVecD m_p;
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
