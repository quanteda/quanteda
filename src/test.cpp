#include "lib.h"
//#include "dev.h"
using namespace quanteda;

// [[Rcpp::export]]
DataFrame cpp_test(const int thread = -1) {

    int G = 100;
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, G), [&](tbb::blocked_range<int> r) {
            for (int g = r.begin(); g < r.end(); ++g) {
            }
        });
    });
    
    // dev::stop_timer("KWIC", timer);
    CharacterVector pre_, keyword_, post_;
    return DataFrame::create(_["pre"]     = pre_,
                             _["keyword"] = keyword_,
                             _["post"]    = post_,
                             _["stringsAsFactors"] = false);
    

}


/***R
quanteda:::cpp_test()
*/
