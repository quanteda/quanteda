#include <Rcpp.h>
using namespace Rcpp;

class Uniform {
public:
  //Uniform(double min_, double max_) : min(min_), max(max_) {}
  Uniform(){}
  
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max );
  }
  double min, max;
};


RCPP_MODULE(unif_module) {
  class_<Uniform>( "Uniform" )
  //.constructor<double,double>()
  .constructor()
  .field( "min", &Uniform::min )
  .field( "max", &Uniform::max )
  .method( "draw", &Uniform::draw )
  ;
}

/*** R
#u <- new( Uniform, 0, 10 )
u <- new( Uniform)
u$draw(10L)
*/