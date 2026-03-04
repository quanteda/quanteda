#include "lib.h"
#include "dev.h"
#include <bitset>
using namespace quanteda;

// #ifdef QUANTEDA
// float GLOBAL_PATTERN_MAX_LOAD_FACTOR = 0.05;
// float GLOBAL_NGRAMS_MAX_LOAD_FACTOR = 0.25;
// #endif

// #if QUANTEDA_USE_TBB
// #if TBB_VERSION_MAJOR >= 2021
// typedef std::atomic<unsigned int> UintAtomic; //oneAPI TBB
// #else
// typedef tbb::atomic<unsigned int> UintAtomic; //old TBB
// #endif
// typedef tbb::concurrent_unordered_map<Ngram, UintAtomic, hash_ngram, equal_ngram> MapNgrams0;
// #else
// typedef std::atomic<unsigned int> UintAtomic;
// typedef std::unordered_map<Ngram, UintAtomic, hash_ngram, equal_ngram> MapNgrams0;
// #endif
typedef std::vector<std::pair<Ngram, unsigned int>> VecNgramsPair;


void counts2(Text text,
             MapNgrams &map_seqs,
             const std::vector<unsigned int> &sizes){
    
    for (auto size : sizes) { // start from the largest size
        if (text.size() < size) continue;
        for (std::size_t i = 0; i < text.size() - size + 1; i++) {
            Text text_sub(text.begin() + i, text.begin() + i + size);
            //Rcout << "@" << i << " " <<  nested << ": ";
            //dev::print_ngram(text_sub);
            auto &count = map_seqs[text_sub];
            count++;
        }
    }
}


/*
 * This function estimates the strength of association between specified words
 * that appear in sequences.
 * @used sequences()
 * @param texts_ tokens object
 * @param count_min sequences appear less than this are ignored
 * @param method method to estimate collocation association
 * @param smoothing this number is added to collocation counts
 * @param nested estimate parameters for nested collocations
 */
// [[Rcpp::export]]
DataFrame cpp_ngrams(const List &texts_,
                     const IntegerVector sizes_,
                     const int thread = -1){
    
    Texts texts = as<Texts>(texts_);
    std::vector<unsigned int> sizes = as< std::vector<unsigned int> >(sizes_);
    std::sort(sizes.begin(), sizes.end(), std::greater<unsigned int>()); // sort in descending order

    MapNgrams map_seqs;
    map_seqs.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    
    //dev::Timer timer;
    //dev::start_timer("Count", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                counts2(texts[h], map_seqs, sizes);
            }
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        counts2(texts[h], map_seqs, sizes);
    }
#endif
    
    std::size_t N = map_seqs.size();
    
    // for estimation
    VecNgramsPair seqs_count; // all the collocation
    seqs_count.reserve(N);
    VecNgramsPair seqs; // only eligible collocation 
    seqs.reserve(N);
    
    // for output
    IntParams counts, lengths;
    counts.reserve(N);
    lengths.reserve(N);
    
    for (auto it = map_seqs.begin(); it != map_seqs.end(); ++it) {
        // convert to a vector for faster iteration
        seqs_count.push_back(std::make_pair(it->first, (unsigned int)it->second));
        
        // NOTE: join all ngrams with max lengths to make matrix
        
        // estimate only sequences without padding
        if (std::none_of(it->first.begin(), it->first.end(), [](unsigned int v){ return v == 0; })) {
            //seqs.push_back(it->first);
            lengths.push_back(it->first.size());
            counts.push_back(it->second);
        }
    }
    
    DataFrame output_ = DataFrame::create(
        //_["collocation"] = seqs_,
        _["count"] = as<IntegerVector>(wrap(counts)),
        _["length"] = as<NumericVector>(wrap(lengths)),
        //_[method] = as<NumericVector>(wrap(lmda)),
        //_["sigma"] = as<NumericVector>(wrap(sgma)),
        _["stringsAsFactors"] = false);
    
    return output_;
    
}


/***R
require(quanteda)
toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
i <- seq_along(types(toks))
i <- seq(1000)
microbenchmark::microbenchmark(
    col = cpp_ngrams(toks, 2, -1),
    exp = expand.grid(i, i),
    times = 10
)
*/
