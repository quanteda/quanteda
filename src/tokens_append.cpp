#include "lib.h"
//#include "dev.h"

using namespace quanteda;

/* 
* Function to append tags
* @used tokens_annocate()
* @creator Kohei Watanabe
* @param tags_ integer IDs of tags
* @param types_ character expressions of tags
* @param bypass_ select documents to modify: TRUE=modify, FALSE=don't modify
*/

// [[Rcpp::export]]
TokensPtr cpp_tokens_append(TokensPtr xptr,
                                 const IntegerVector &tags_,
                                 const CharacterVector &types_,
                                 const LogicalVector bypass_,
                                 const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = Rcpp::as<Types>(types_);
    
    if (tags_.size() != (int)texts.size())
        throw std::range_error("Invalid tags");
    
    if (bypass_.size() != (int)texts.size())
        throw std::range_error("Invalid bypass");
    std::vector<bool> bypass = Rcpp::as< std::vector<bool> >(bypass_);
    std::vector<unsigned int> tags = Rcpp::as< std::vector<unsigned int> >(tags_);
    
    unsigned int id_max = xptr->types.size();
    xptr->types.insert(xptr->types.end(), types.begin(), types.end());
    
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                if (!bypass[h])
                    xptr->texts[h].push_back(tags[h] + id_max);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        if (!bypass[h])
            xptr->texts[h].push_back(tags[h] + id_max);
    }
#endif
    
    return xptr;
}

/***R

toks <- quanteda::tokens(c("a b c", "d e f"), xptr = TRUE)
cpp_tokens_append(toks, 1:2, c("XX", "YY"), c(FALSE, TRUE))

*/
