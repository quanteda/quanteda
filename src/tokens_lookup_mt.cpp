//#include "dev.h"
#include "quanteda.h"
using namespace quanteda;

Text lookup(Text tokens, 
            const std::vector<std::size_t> &spans,
            const unsigned int &id_max,
            const bool &overlap,
            const MultiMapNgrams &map_keys){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    // Make flags for all the keys
    std::vector< std::vector< bool > > flags_match(id_max);
    if (!overlap) {
        std::vector< bool > flags_init(tokens.size(), false);
        for (unsigned int h = 0; h < id_max; h++) {
            flags_match[h] = flags_init;
        }
    }
    
    std::size_t match = 0;
    std::vector< std::vector<unsigned int> > keys(tokens.size()); 
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_keys.equal_range(ngram);
            for (auto it = range.first; it != range.second; ++it){
                //Rcout << it->second << "\n";
                unsigned int id = it->second;
                if (!overlap) {
                    std::vector< bool > &flags_temp = flags_match[id - 1];
                    bool flaged = std::any_of(flags_temp.begin() + i, flags_temp.begin() + i + span, [](bool v) { return v; });
                    if (!flaged) {
                        keys[i].push_back(id); // keep multiple keys in the same position
                        std::fill(flags_temp.begin() + i, flags_temp.begin() + i + span, true); // mark tokens matched
                        match++;
                    }
                } else {
                    keys[i].push_back(id); // keep multiple keys in the same position
                    match++;
                }
            }
        }
    }
    
    if (match == 0) return {}; // return empty vector if no match
    
    // Flatten the vector of vector
    Text keys_flat;
    keys_flat.reserve(match);
    for (auto &key_sub: keys) {
        if (key_sub.size() > 1) {
            std::sort(key_sub.begin(), key_sub.end()); // sort in order of keys
        }
        keys_flat.insert(keys_flat.end(), key_sub.begin(), key_sub.end());                                                                                         
    }
    return keys_flat;
}


struct lookup_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<std::size_t> &spans;
    const unsigned int &id_max;
    const bool &overlap;
    const MultiMapNgrams &map_keys;
    
    // Constructor
    lookup_mt(Texts &input_, Texts &output_, const std::vector<std::size_t> &spans_, const unsigned int &id_max_,
              const bool &overlap_, const MultiMapNgrams &map_keys_):
        input(input_), output(output_), spans(spans_), id_max(id_max_), overlap(overlap_), map_keys(map_keys_){}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            output[h] = lookup(input[h], spans, id_max, overlap, map_keys);
        }
    }
};

/* 
* This funciton finds features in tokens object. This is similar to tokens_replace, 
* but all overlapping or nested features are detected and recorded by IDs.
* The number of threads is set by RcppParallel::setThreadOptions()
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param words_ list of features to find
* @param ids_ IDs of features
* @param overlap if count overlapped words if true
*/


// [[Rcpp::export]]
List qatd_cpp_tokens_lookup(const List &texts_, 
                            const List &keys_,
                            const IntegerVector &ids_,
                            const bool overlap){
    
    Texts input = Rcpp::as<Texts>(texts_);
    const unsigned int id_max = Rcpp::max(ids_);
    
    MultiMapNgrams map_keys;
    std::vector<std::size_t> spans(keys_.size());
    for (unsigned int g = 0; g < (unsigned int)keys_.size(); g++) {
        if (has_na(keys_[g])) continue;
        Ngram word = keys_[g];
        map_keys.insert(std::make_pair(word, ids_[g]));
        spans[g] = word.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Dictionary lookup", timer);
#if QUANTEDA_USE_TBB
    lookup_mt lookup_mt(input, output, spans, id_max, overlap, map_keys);
    parallelFor(0, input.size(), lookup_mt);
#else
    for (std::size_t h = 0; h < input.size(); h++) {
        output[h] = lookup(input[h], spans, id_max, overlap, map_keys);
    }
#endif
    // dev::stop_timer("Dictionary lookup", timer);
    ListOf<IntegerVector> output_ = Rcpp::wrap(output);
    return output_;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
dict <- list(5, c(5, 6) , 4)
#dict <- list(1, 10, 20)
#keys <- 1:length(dict)
keys <- rep(1, length(dict))
qatd_cpp_tokens_lookup(toks, dict, keys, FALSE)
qatd_cpp_tokens_lookup(toks, dict, keys, TRUE)


*/
