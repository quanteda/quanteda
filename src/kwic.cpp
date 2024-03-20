#include "lib.h"
//#include "dev.h"
using namespace quanteda;

std::string kwic(Text tokens, 
                 const std::vector<std::string> &types,
                 const std::string delim,
                 int from,
                 int to){
    
    if (tokens.empty()) return ""; // return empty vector for empty text
    // positions are 1-based
    from = std::max(from - 1, 0);
    to = std::min(to, (int)tokens.size());
    //Rcout << from << ", " << to << "\n";
    if (from > to)
        throw std::range_error("Invalid index");
    Text context(tokens.begin() + from, tokens.begin() + to);
    return join_strings(context, types, delim);
}


/* 
 * Function to create kwic
 * @used kwic()
 * @creator Kohei Watanabe
 * @param documents_ document index 
 * @param pos_from_ starting position of keywords (1-based)
 * @param pos_to_ ending position of keywords (1-based)
 * @param window size of the context window
 * @param delim_ characters to join tokens
 */


// [[Rcpp::export]]
DataFrame cpp_kwic(TokensPtr xptr,
                   const IntegerVector documents_,
                   const IntegerVector pos_from_,
                   const IntegerVector pos_to_,
                   const int window,
                   const String delim_ = " ",
                   const int thread = -1) {

    Texts texts = xptr->texts;
    Types types = xptr->types;
    std::string delim = delim_;
    
    if (pos_from_.size() != documents_.size())
        throw std::range_error("Invalid pos_from");
    if (pos_to_.size() != documents_.size())
        throw std::range_error("Invalid pos_to");
    
    std::vector<int> documents = Rcpp::as< std::vector<int> >(documents_);
    std::vector<int> pos_from = Rcpp::as< std::vector<int> >(pos_from_);
    std::vector<int> pos_to = Rcpp::as< std::vector<int> >(pos_to_);
    
    // dev::Timer timer;
    // dev::start_timer("Kwic", timer);
    std::size_t G = documents.size();
    std::size_t H = texts.size();
    std::vector<std::string> pre(G), keyword(G), post(G);
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, G), [&](tbb::blocked_range<int> r) {
            for (int g = r.begin(); g < r.end(); ++g) {
                int h = documents[g] - 1;
                if (h < 0 || (int)H <= h)
                    throw std::range_error("Invalid documents");
                keyword[g] = kwic(texts[h], types, delim, pos_from[g], pos_to[g]);
                pre[g] = kwic(texts[h], types, delim, pos_from[g] - window, pos_from[g] - 1L);
                post[g] = kwic(texts[h], types, delim, pos_to[g] + 1L, pos_to[g] + window);
            }
        });
    });
    
#else
    for (int g = 0; g < (int)G; g++) {
        int h = documents[g] - 1L;
        if (h < 0 || (int)H <= h)
            throw std::range_error("Invalid documents");
        keyword[g] = kwic(texts[h], types, delim, pos_from[g], pos_to[g]);
        pre[g] = kwic(texts[h], types, delim, pos_from[g] - window, pos_from[g] - 1L);
        post[g] = kwic(texts[h], types, delim, pos_to[g] + 1L, pos_to[g] + window);
    }
#endif
    
    // dev::stop_timer("KWIC", timer);
    
    CharacterVector pre_ = encode(pre);
    CharacterVector keyword_ = encode(keyword);
    CharacterVector post_ = encode(post);

    return DataFrame::create(_["pre"]     = pre_,
                             _["keyword"] = keyword_,
                             _["post"]    = post_,
                             _["stringsAsFactors"] = false);
    

}


/***R
#toks <- quanteda::tokens(c("a b c d e f g", "A B C D E F G"), xptr = TRUE)
toks <- quanteda::tokens(c("a b c d 😊 f g", "A B C D E 🎅 G"), xptr = TRUE)
toks
#cpp_kwic(toks, c(1, 2), c(3, 7), c(4, 8), 2, "_")
cpp_kwic(toks, c(1), c(3), c(4), 2, "_")
*/
