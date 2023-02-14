#include "tokens.h"
//#include "dev.h"
using namespace quanteda;


/* 
 * This function split tokens into segments by given patterns
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_segment()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param size size of chunks
 * @param overlap number of tokens overlapping
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_combine(TokensPtr xptr1, TokensPtr xptr2){
    
    Types types;
    types.reserve(xptr1->types.size() + xptr2->types.size());
    types.insert(types.end(), xptr1->types.begin(), xptr1->types.end());
    types.insert(types.end(), xptr2->types.begin(), xptr2->types.end());
    
    std::size_t H = xptr2->texts.size(); 
    std::size_t V = xptr1->types.size();
    
    Texts texts = xptr2->texts;
#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            for (std::size_t i = 0; i < xptr2->texts[h].size(); i++) {
                texts[h][i] += V;
            }
        }
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        for (std::size_t i = 0; i < xptr2->texts[h].size(); i++) {
            texts[h][i] += V;
        }
    }
#endif

    Texts temp;
    temp.reserve(xptr1->texts.size() + texts.size());
    temp.insert(temp.end(), xptr1->texts.begin(), xptr1->texts.end());
    temp.insert(temp.end(), texts.begin(), texts.end());
    
    TokensObj *ptr = new TokensObj(temp, types, true, true);
    return TokensPtr(ptr, true);
}

/***R

toks1 <- tokens(c(d1 = "a b c", d2 = "a", d3 = "d"))
xtoks1 <- as.tokens_xptr(toks1)
toks2 <- tokens(c(d4 = "a b", d5 = " l m", d6 = "x y z"))
xtoks2 <- as.tokens_xptr(toks2)

out <- cpp_tokens_combine(xtoks1, xtoks2)


*/
