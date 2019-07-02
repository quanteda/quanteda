#include "lib.h"
#include "recompile.h"
//#include "dev.h"
using namespace quanteda;

Texts chunk(Text &tokens,
            UintParam &count,
            const int size,
            const int overlap){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::size_t step;
    Texts chunks;
    step = size - overlap;
    chunks.reserve(ceil((double)tokens.size() / step));
    for (size_t i = 0; i < tokens.size(); i += step) {
        Text chunk(tokens.begin() + i, tokens.begin() + std::min(i + size, tokens.size()));
        chunks.push_back(chunk);
        count++;
    }
    return chunks;
}

struct chunk_mt : public Worker{
    
    Texts &texts;
    std::vector<Texts> &temp;
    UintParam &count;
    const int size;
    const int overlap;
    
    chunk_mt(Texts &texts_, std::vector<Texts> &temp_, UintParam &count_, const int size_, 
             const int overlap_):
             texts(texts_), temp(temp_), count(count_), size(size_), 
             overlap(overlap_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++){
            temp[h] = chunk(texts[h], count, size, overlap);
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
 * @param overlap number of tokens overlapping
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_chunk(const List &texts_,
                           const CharacterVector types_,
                           const int size,
                           const int overlap){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    UintParam count = 0;
    // dev::Timer timer;
    std::vector<Texts> temp(texts.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
     chunk_mt chunk_mt(texts, temp, count, size, overlap);
     parallelFor(0, texts.size(), chunk_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        temp[h] = chunk(texts[h], count, size, overlap);
    }
#endif
    
    Texts chunks(count);
    IntegerVector docnum_(count), segnum_(count);

    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        for (size_t i = 0; i < temp[h].size(); i++) {
            chunks[j] = temp[h][i];
            docnum_[j] = (int)h + 1;
            segnum_[j] = (int)i + 1;
            j++;
        }
    }
    
    Tokens chunks_ = recompile(chunks, types, false, false, is_encoded(types_));
    chunks_.attr("docnum") = docnum_;
    chunks_.attr("segnum") = segnum_;
    
    return(chunks_);
}

/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 100)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
out1 <- qatd_cpp_tokens_chunk(toks, letters, 2, 0)
out2 <- qatd_cpp_tokens_chunk(toks, letters, 2, 1)

out3 <- qatd_cpp_tokens_chunk(toks, letters, 2, 0)
out4 <- qatd_cpp_tokens_chunk(toks, letters, 2, 1)


*/
