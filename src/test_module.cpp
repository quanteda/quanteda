#include <Rcpp.h>
using namespace Rcpp;

std::string hello() {
  return
  "hello";
}
int bar( int x) {
  return
  x*2;
}
double foo( int x, double y) {
  return
  x * y;

}

RCPP_MODULE(yada) {
  using namespace
  Rcpp;
  function("hello" , &hello);
  function("bar"   , &bar  );
  function("foo"   , &foo  );
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

# yd <- Module("yada")
# yd$bar(2L)
# yd$foo(2L, 10.0)
#  

*/
