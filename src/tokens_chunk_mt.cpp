#include "quanteda.h"
#include "recompile.h"
//#include "dev.h"
using namespace quanteda;

Texts chunk(Text tokens,
            const int size,
            const bool overlap){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::size_t step;
    Texts chunks;
    if (overlap) {
        step = 1;
        chunks.reserve(tokens.size());
    } else {
        step = size;
        chunks.reserve(ceil(tokens.size() / size));
    }
    for (size_t i = 0; i < tokens.size(); i += step) {
        Text chunk(tokens.begin() + i, tokens.begin() + min(i + size, tokens.size()));
        chunks.push_back(chunk);
    }
    return chunks;
}

struct chunk_mt : public Worker{
    
    Texts &texts;
    std::vector<Texts> &temp;
    const int size;
    const bool overlap;
    
    chunk_mt(Texts &texts_, std::vector<Texts> &temp_, const int size_, const bool overlap_):
             texts(texts_), temp(temp_), size(size_), overlap(overlap_){}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            temp[h] = chunk(texts[h], size, overlap);
        }
    }
};


/* 
 * This function split tokens into segments by given patterns
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_segment()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param size size of chunks
 * @param overlap chunks are overlapped with next chunks if TRUE
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_chunk(const List &texts_,
                           const CharacterVector types_,
                           const int size,
                           const bool overlap){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    
    // dev::Timer timer;
    std::vector<Texts> temp(texts.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
     chunk_mt chunk_mt(texts, temp, size, overlap);
     parallelFor(0, texts.size(), chunk_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = chunk(texts[h], size, overlap);
    }
#endif
    
    // Get total number of matches
    std::size_t len = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        len += temp[h].size();
    }
    Texts chunks(len);
    IntegerVector docnum_(len), segnum_(len);

    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        for (size_t i = 0; i < temp[h].size(); i++) {
            chunks[j] = temp[h][i];
            docnum_[j] = (int)h + 1;
            segnum_[j] = (int)i + 1;
            j++;
        }
    }
    
    Tokens chunks_ = recompile(chunks, types, false, false, false);
    chunks_.attr("docnum") = docnum_;
    chunks_.attr("segnum") = segnum_;
    
    return(chunks_);
}

/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 100)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
out1 <- qatd_cpp_tokens_chunk(toks, letters, 2, FALSE)
out2 <- qatd_cpp_tokens_chunk(toks, letters, 2, TRUE)

*/
