#include "lib.h"
//#include "dev.h"
using namespace quanteda;


/* 
 * Function to create kwic
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used kwic()
 * @creator Kohei Watanabe
 * @param documents_ document index 
 * @param pos_from_ starting position of keywords (1-based)
 * @param pos_to_ ending position of keywords (1-based)
 * @param window size of the context window
 * @param delim_ characters to join tokens
 */


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
# oks <- quanteda::tokens(c("a b c d e f g", "A B C D E F G"), xptr = TRUE)
# toks <- quanteda::tokens(c("a b c d 😊 f g", "A B C D E 🎅 G"), xptr = TRUE)
# toks
# cpp_kwic(toks, c(1, 2), c(3, 7), c(4, 8), 2, "_")
# cpp_kwic(toks, c(1), c(3), c(4), 2, "_")

quanteda:::cpp_test()
*/
