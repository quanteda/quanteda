#include "lib.h"
//#include "dev.h"
using namespace quanteda;


/* 
 * Function to group documents
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_group()
 * @creator Kohei Watanabe
 * @param groups_ group index
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_group(TokensPtr xptr,
                           IntegerVector groups_,
                           const int thread = -1) {
    
    Texts texts = xptr->texts;
    std::vector<int> groups = Rcpp::as< std::vector<int> >(groups_);
    
    if (texts.size() != groups.size())
        throw std::range_error("Invalid groups");
    
    CharacterVector levels_ = groups_.attr("levels");
    int G = levels_.size();
    Texts temp(G);

#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, G), [&](tbb::blocked_range<int> r) {
            for (int g = r.begin(); g < r.end(); ++g) {
                for (std::size_t h = 0; h < texts.size(); h++) {
                    if (g + 1 == groups[h]) {
                        temp[g].insert(temp[g].end(), texts[h].begin(), texts[h].end());
                    }
                }
            }
        });
    });
#else
    for (std::size_t g = 0; g < G; g++) {
        for (std::size_t h = 0; h < texts.size(); h++) {
            if (g + 1 == groups[h]) {
                temp[g].insert(temp[g].end(), texts[h].begin(), texts[h].end());
            }
        }
    }
#endif
    
    xptr->texts = temp;
    return xptr;
}

/***R

toks <- tokens(c("a b c", "a", "d"))
xtoks <- as.tokens_xptr(toks)
g <- factor(paste0("doc", c(1, 2, 2)), levels = paste0("doc", 1:3))
out <- cpp_tokens_group(xtoks, g)


*/
