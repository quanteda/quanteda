#include "lib.h"
//#include "dev.h"
using namespace quanteda;

struct hash_pair {
  size_t operator()(const std::pair<unsigned int, unsigned int> &p) const {
    
    unsigned int seed = 0;
    seed = p.first;
    seed += p.second << 16;
    return std::hash<unsigned int>()(seed);
  }
};

struct equal_pair {
  bool operator() (const std::pair<unsigned int, unsigned int> &p1,
                   const std::pair<unsigned int, unsigned int> &p2) const {
    return (p1.first == p2.first && p1.second == p2.second);
  }
};


typedef std::unordered_set<std::pair<unsigned int, unsigned int>, 
                           hash_pair, equal_pair> SetPair;

// find out if a pair of token exists in a document
bool exist(const unsigned int &x, const unsigned int &y,
           SetPair &set_pair){
    
    auto it = set_pair.insert(std::make_pair(x, y));
    return !it.second;
}

//count the co-occurance when count is set to "frequency" or "weighted"
void count_col(const Text &text,
               const std::vector<double> &weights,    
               const unsigned int &window,
               const bool &ordered,
               const bool &boolean,
               Triplets &fcm_tri) {
    
    SetPair set_pair;
    set_pair.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);
    
    unsigned int j_ini, j_lim;
    double weight;
    for (unsigned int i = 0; i < text.size(); i++) {
        if (text[i] == 0) continue; // skip padding
        j_ini = std::min((int)(i + 1), (int)text.size());
        j_lim = std::min((int)(i + window + 1), (int)text.size());
        for (unsigned int j = j_ini; j < j_lim; j++) {
            if (text[j] == 0) continue; // skip padding
            weight = weights[std::abs((int)j - (int)i) - 1];
            if (ordered) {
                if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                    //Rcout << i << " " << j << "\n";
                    Triplet tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight);
                    fcm_tri.push_back(tripl);
                }
            } else {
                if (text[i] < text[j]) {
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        Triplet tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight);
                        fcm_tri.push_back(tripl);
                    }
                } else if (text[i] > text[j]){
                    if (!boolean || !exist(text[j] - 1, text[i] - 1, set_pair)) {
                        Triplet tripl = std::make_tuple(text[j] - 1, text[i] - 1, weight);
                        fcm_tri.push_back(tripl);
                    }
                } else {
                    if (!boolean || !exist(text[i] - 1, text[j] - 1, set_pair)) {
                        Triplet tripl = std::make_tuple(text[i] - 1, text[j] - 1, weight * 2);
                        fcm_tri.push_back(tripl);
                    }
                } 
            }
        }
    }
}

// [[Rcpp::export]]
S4 cpp_fcm(TokensPtr xptr,
           const int n_types,
           const NumericVector &weights_,
           const bool boolean,
           const bool ordered,
           const int thread = -1) {
    
    // triplets are constructed according to tri & ordered settings to be efficient
    xptr->recompile();
    Texts texts = xptr->texts;
    Types types = xptr->types;
    std::vector<double> weights = Rcpp::as< std::vector<double> >(weights_);
    unsigned int window = weights.size();

    // declare the vector returned by parallelized procedure
    Triplets fcm_tri;

    //dev::Timer timer;
    //dev::start_timer("Count", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                count_col(texts[h], weights, window, ordered, boolean, fcm_tri);
            }    
        });
    });
#else        
    for (std::size_t h = 0; h < H; h++) {
        count_col(texts[h], weights, window, ordered, boolean, fcm_tri);
    }
#endif
    
    //dev::stop_timer("Count", timer);
    //dev::start_timer("Convert", timer);
    return to_matrix(fcm_tri, n_types, n_types, false);
}


/***R
toks <- list(rep(1:10, 10), rep(5:15, 10))
toks <- list(c(1, 4, 2, 3))
types <- unique(unlist(toks))
cpp_fcm(toks, max(types), c(1, 1), FALSE, TRUE)
cpp_fcm(toks, max(types), c(1, 1), FALSE, FALSE)

# for c(1, 2, 2, 3), window = 2
# 0 2 0
# 2 0 2
# 0 2 0


*/
