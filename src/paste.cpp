#include "lib.h"
using namespace quanteda;

// [[Rcpp::export]]
CharacterVector cpp_paste(const CharacterVector x_, 
                          const CharacterVector y_, 
                          const std::string sep = " ",
                          const int thread = -1) {
    
    if (x_.size() != y_.size())
        throw std::length_error("The lengths of x and y must be the same");
    
    const std::vector<std::string> x = Rcpp::as< std::vector<std::string> >(x_);
    const std::vector<std::string> y = Rcpp::as< std::vector<std::string> >(y_);
    const std::size_t H = x.size();
    std::vector<std::string> z(H);

#if QUANTEDA_USE_TBB
    //Rcout << "here\n";
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                z[h] = x[h] + sep + y[h];
            }    
        }, tbb::static_partitioner()); // minimize overhead
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        z[h] = x[h] + sep + y[h];
    }
#endif
    return wrap(z);
}

// // [[Rcpp::export]]
// CharacterVector test1(const CharacterVector x_) {
//     const std::vector<std::string> x = Rcpp::as< std::vector<std::string> >(x_);
//     return wrap(x);
// }
// 
// // [[Rcpp::export]]
// CharacterVector test2(const CharacterVector x_) {
//     const std::vector<std::string> x = Rcpp::as< std::vector<std::string> >(x_);
//     return Rcpp::CharacterVector(x.begin(), x.end());
// }

/***R
segid <- rep(1:10, 10000000)
docid <- factor(rep(1:10000000, each = 10))
microbenchmark::microbenchmark(
    paste0(docid, ".", segid),
    quanteda:::cpp_paste(docid, segid, sep = "."),
    #test1(docid),
    #test2(docid),
    times = 5
)
*/

