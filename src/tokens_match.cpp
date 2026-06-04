#include "lib.h"
//#include "dev.h"

using namespace quanteda;

Text match(Text tokens, 
           std::vector<int> &ids){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    Text tokens_new;
    tokens_new.reserve(tokens.size());
    for (std::size_t i = 0; i < tokens.size(); i++) {
        //Rcout << tokens[i] << ": ";
        if (tokens[i] <= 0) {
            //Rcout << 0 << "\n";
            tokens_new.push_back(0);
        } else if (tokens[i] < ids.size()) {
            int id = ids[tokens[i] - 1];
            //Rcout << id << "\n";
            if (id >= 0)
                tokens_new.push_back(id);
        }
    }
    return tokens_new;
}

/* 
* Function to replace tokens
* @used tokens_replace()
* @creator Kohei Watanabe
* @param patterns_ IDs of patterns
* @param replacements_ IDs to replace patterns. Must be the same length as patterns_
* @param mode 1: keep unmatched; 2: remove unmatched
*/


// [[Rcpp::export]]
TokensPtr cpp_tokens_match(TokensPtr xptr,
                           const IntegerVector &ids_,
                           const int thread = -1) {
    
    Texts texts = xptr->texts;
    std::vector<int> ids = Rcpp::as< std::vector<int> >(ids_);
    
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                texts[h] = match(texts[h], ids);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        texts[h] = match(texts[h], ids);
    }
#endif
    xptr->texts = texts;
    xptr->recompiled = true; // avoid changes in token IDs
    return xptr;
}

/***R

lis <- list(letters[1:10], letters[10:15])
toks <- as.tokens_xptr(as.tokens(c(lis)))
index <- rev(seq(0, 15))
index <- 6:10

cpp_tokens_match(toks, index)

*/
