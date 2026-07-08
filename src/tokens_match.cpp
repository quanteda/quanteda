#include "lib.h"
//#include "dev.h"

using namespace quanteda;

Text match_type(const Text &tokens, 
                const std::vector<int> &ids){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    Text tokens_new;
    tokens_new.reserve(tokens.size());
    for (std::size_t i = 0; i < tokens.size(); i++) {
        //Rcout << tokens[i] << ": ";
        if (tokens[i] == 0) {
            //Rcout << 0;
            tokens_new.push_back(0);
        } else if (tokens[i] <= ids.size()) {
            int id = ids[tokens[i] - 1];
            //Rcout << id;
            if (id >= 0)
                tokens_new.push_back(id);
        }
        //Rcout << "\n";
    }
    return tokens_new;
}

/* 
* Function to reassign tokens IDs
* @used tokens_match()
* @creator Kohei Watanabe
* @param ids_ token IDs to be reassigned. Tokens with negative IDs are removed. 
*/


// [[Rcpp::export]]
TokensPtr cpp_tokens_match(TokensPtr xptr,
                           const IntegerVector &ids_,
                           const int thread = -1) {
    
    std::vector<int> ids = Rcpp::as< std::vector<int> >(ids_);
    
    std::size_t H = xptr->texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                xptr->texts[h] = match_type(xptr->texts[h], ids);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        xptr->texts[h] = match_type(xptr->texts[h], ids);
    }
#endif
    xptr->recompiled = true; // avoid changes in token IDs
    return xptr;
}

/***R

toks <- tokens(c("a b c", "b b b a", "c c b"), xptr = TRUE)
quanteda:::cpp_as_list(toks)

id <- match(c("a", "b", "c"), quanteda:::cpp_get_types(toks))
match(c("a", "b", "c"), quanteda:::cpp_get_types(toks))
id <- match(quanteda:::cpp_get_types(toks), c("a", "b", "c"))
id[is.na(id)] <- -1
cpp_tokens_match(toks, id)
quanteda:::cpp_set_types(toks, c("a", "b", "c"))
*/
