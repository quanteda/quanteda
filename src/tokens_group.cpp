#include "lib.h"
//#include "dev.h"
using namespace quanteda;


/* 
 * Function to group documents
 * @used tokens_group()
 * @creator Kohei Watanabe
 * @param groups_ group index
 */

typedef std::vector<int> Group;
typedef std::vector<Group> Groups;

// [[Rcpp::export]]
TokensPtr cpp_tokens_group(TokensPtr xptr,
                           List groups_,
                           const int thread = -1) {
    
    Texts texts = xptr->texts;
    Groups groups = Rcpp::as<Groups>(groups_);

    // pre-allocate memory
    std::size_t G = groups.size();
    std::size_t H = texts.size();
    std::vector<size_t> sizes(G);
    
    Texts temp(G);
    for (std::size_t g = 0; g < G; g++) {
        std::size_t size = 0;
        for (std::size_t h: groups[g]) {
            if (h < 1 || H < h)
                throw std::range_error("Invalid groups");
            size += texts[h - 1].size();
        }
        temp[g].reserve(size);  
    }

#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
       tbb::parallel_for(tbb::blocked_range<int>(0, G), [&](tbb::blocked_range<int> r) {
          for (int g = r.begin(); g < r.end(); ++g) {
              for (std::size_t h: groups[g]) {
                  temp[g].insert(temp[g].end(), texts[h - 1].begin(), texts[h - 1].end());
              }
          }
       });
    });
#else
    for (std::size_t g = 0; g < G; g++) {
        for (std::size_t h: groups[g]) {
            temp[g].insert(temp[g].end(), texts[h - 1].begin(), texts[h - 1].end());
        }
    }
#endif
    
    TokensObj *ptr_new = new TokensObj(temp, xptr->types, xptr->recompiled);
    TokensPtr xptr_new = TokensPtr(ptr_new, true);
    
    return xptr_new;
}

/***R

toks <- tokens(c("a b c", "a", "d"))
xtoks <- as.tokens_xptr(toks)
g <- factor(paste0("doc", c(1, 2, 2)), levels = paste0("doc", 1:3))
out <- cpp_tokens_group(xtoks, g)


*/
