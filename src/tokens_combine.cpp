#include "lib.h"
//#include "dev.h"
using namespace quanteda;


/* 
 * Function to combine tokens objects
 * @used c()
 * @creator Kohei Watanabe
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_combine(TokensPtr xptr1, 
                             TokensPtr xptr2,
                             const int thread = -1) {
    
    //dev::Timer timer;

    std::size_t V = xptr1->types.size();
    std::size_t W = xptr2->types.size();
    std::size_t G = xptr1->texts.size(); 
    std::size_t H = xptr2->texts.size(); 
    
    xptr1->texts.resize(G + H);
    xptr1->types.reserve(V + W);
    xptr1->types.insert(xptr1->types.end(), xptr2->types.begin(), xptr2->types.end());

    //dev::start_timer("Combine", timer);
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                Text text = xptr2->texts[h];
                for (std::size_t i = 0; i < text.size(); i++) {
                    if (text[i] != 0)
                        text[i] += V;
                }
                xptr1->texts[G + h] = text;
            }
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        Text text = xptr2->texts[h];
        for (std::size_t i = 0; i < text.size(); i++) {
            if (text[i] != 0)
                text[i] += V;
        }
        xptr1->texts[G + h] = text;
    }
#endif
    //dev::stop_timer("Combine", timer);
    return xptr1;
}

/***R

toks1 <- tokens(c(d1 = "a b c", d2 = "a", d3 = "d"))
xtoks1 <- as.tokens_xptr(toks1)
toks2 <- tokens(c(d4 = "a b", d5 = " l m", d6 = "x y z"))
xtoks2 <- as.tokens_xptr(toks2)

out <- cpp_tokens_combine(xtoks1, xtoks2)


*/
