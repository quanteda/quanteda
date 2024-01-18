#include "lib.h"
//#include "dev.h"
//#include "utf8.h"
using namespace quanteda;

// [[Rcpp::export]]
int cpp_test(const int x,
                     const int thread = -1) {

    std::size_t H = 100;
//#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                Rcout << "h:" << h << "\n";
                //index_types(confs[h], types, index);
            }
        });
    });
// #else
//     for (size_t h = 0; h < H; h++) {
//         index_types(confs[h], types, index);
//     }
// #endif

    return x;
}

/*** R
#cpp_index_types(c("a*", "*b", "*c*"), 
#                c("bbb", "aaa", "ccc", "aa", "bb"))
#cpp_index_types(c("跩", "跩*"), c("跩购鹇", "跩"))

load("../vignettes/pkgdown/replication/data_char_mobydick.rda")
xtoks <- quanteda:::tokens(data_char_mobydick, xptr = TRUE)
type <- quanteda:::cpp_get_types(xtoks)
cpp_test(100)
#cpp_index_types("whale*", type, TRUE, 2)

*/
