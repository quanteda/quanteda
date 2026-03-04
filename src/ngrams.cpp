#include "lib.h"
//#include "dev.h"
using namespace quanteda;

void counts(Text text,
            MapNgrams &map_seqs,
            const unsigned int &size){
    
    if (text.size() < size) return;
    for (std::size_t i = 0; i < text.size() - size + 1; i++) {
        Text text_sub(text.begin() + i, text.begin() + i + size);
        auto &count = map_seqs[text_sub];
        count++;
    }
}


/*
 * This function create a matrix that lists all the n-grams
 * @used sequences()
 * @param texts_ tokens object
 * @param size the size of the n-grams
 */
// [[Rcpp::export]]
IntegerVector cpp_ngrams(const List &texts_,
                         unsigned int size,
                         const int thread = -1){
    
    Texts texts = as<Texts>(texts_);

    MapNgrams map_seqs;
    map_seqs.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                counts(texts[h], map_seqs, size);
            }
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        counts(texts[h], map_seqs, size);
    }
#endif
    
    std::size_t N = map_seqs.size();
    std::vector<int> vec;
    vec.reserve(N * size);
    
    for (auto it = map_seqs.begin(); it != map_seqs.end(); ++it) {
        vec.insert(vec.end(), it->first.begin(), it->first.end());
    }
    
    IntegerVector vec_ = wrap(vec);
    vec_.attr("dim") = Dimension(size, N);
    IntegerMatrix mat_ = transpose(as<IntegerMatrix>(vec_));
    return mat_;
    
}


/***R
require(quanteda)
toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
i <- seq_along(types(toks))
microbenchmark::microbenchmark(
    col = cpp_ngrams(toks, 3, -1),
    exp = expand.grid(1:2, i, 1:2),
    times = 10
)
*/
