//#include "dev.h"
#include "quanteda.h"
#include "recompile.h"
using namespace quanteda;

Text lookup(Text tokens, 
            const std::vector<std::size_t> &spans,
            const unsigned int &id_max,
            const bool &overlap,
            const int &nomatch,
            const MultiMapNgrams &map_keys){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    // Match flag for each token
    std::vector<bool> flags_match_any(tokens.size(), false);
    
    // Match flag for each token for each key
    std::vector< std::vector<bool> > flags_match(id_max);
    if (!overlap) {
        std::vector<bool> flags_init(tokens.size(), false);
        for (unsigned int h = 0; h < id_max; h++) {
            flags_match[h] = flags_init;
            //Rcout << "h " << h << "\n";
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
                //Rcout << "id " << id << "\n";
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
                    flags_match_any[i] = true;
                    match++;
                }
            }
        }
    }
    
    if (match == 0) {
        if (nomatch == 0) {
            return {}; // return empty vector if no match
        } else if (nomatch == 1) {
            Text keys_flat(tokens.size(), id_max + 1); 
            return keys_flat;
        } else if (nomatch == 2) {
            return tokens; // do nothing in exclusive mode
        }
    }
    
    // Merge match flags
    if (!overlap) {
        for (size_t h = 0; h < flags_match.size(); h++) {
            for (size_t i = 0; i < flags_match[h].size(); i++) {
                if (flags_match[h][i]) {
                    flags_match_any[i] = true;
                }
            }
        }
    }
    
    // Flatten the vector of vector
    Text keys_flat;
    if (nomatch > 0) {
        keys_flat.reserve(match + tokens.size());
    } else {
        keys_flat.reserve(match);
    }
    for (size_t i = 0; i < keys.size(); i++) {
        if (flags_match_any[i]) {
            std::vector<unsigned int> key_sub = keys[i];
            if (key_sub.size() > 1) {
                std::sort(key_sub.begin(), key_sub.end()); // sort in order of keys
            }
            keys_flat.insert(keys_flat.end(), key_sub.begin(), key_sub.end());
        } else {
            if (nomatch == 1) {
                keys_flat.push_back(id_max + 1); // pad with a new ID
            } else if (nomatch == 2) {
                keys_flat.push_back(id_max + tokens[i]); // keep original token
            }
        }
    }
    return keys_flat;
}


struct lookup_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const unsigned int &id_max;
    const bool &overlap;
    const int &nomatch;
    const MultiMapNgrams &map_keys;
    
    // Constructor
    lookup_mt(Texts &texts_, const std::vector<std::size_t> &spans_, const unsigned int &id_max_,
              const bool &overlap_, const int &nomatch_, const MultiMapNgrams &map_keys_):
              texts(texts_), spans(spans_), id_max(id_max_), overlap(overlap_), nomatch(nomatch_),
              map_keys(map_keys_){}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            texts[h] = lookup(texts[h], spans, id_max, overlap, nomatch, map_keys);
        }
    }
};

/* 
* This funciton finds patterns in tokens object. This is similar to tokens_replace, 
* but all overlapping or nested patterns are detected and recorded by IDs.
* The number of threads is set by RcppParallel::setThreadOptions()
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param words_ list of patterns to find
* @param ids_ IDs of patterns
* @param overlap if count overlapped words if true
* @param nomatch determine how to treat unmached words: (0) ignore, (1) keep; (3) pad
*/


// [[Rcpp::export]]
List qatd_cpp_tokens_lookup(const List &texts_,
                            const CharacterVector types_,
                            const List &keys_,
                            const IntegerVector &ids_,
                            const bool overlap,
                            const int nomatch){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    unsigned int id_max(0);
    if (ids_.size() > 0) id_max = Rcpp::max(ids_);
    // Rcout << id_max << "\n";
    
    //dev::Timer timer;
    
    //dev::start_timer("Map construction", timer);

    MultiMapNgrams map_keys;
    map_keys.max_load_factor(GLOBAL_PATTERNS_MAX_LOAD_FACTOR);
    
    Ngrams keys = Rcpp::as<Ngrams>(keys_);
    std::vector<unsigned int> ids = Rcpp::as< std::vector<unsigned int> >(ids_);
    std::vector<std::size_t> spans(keys.size());
    for (size_t g = 0; g < std::min(keys.size(), ids.size()); g++) {
        Ngram key = keys[g];
        unsigned int id = ids[g];
        map_keys.insert(std::pair<Ngram, unsigned int>(key, id));
        spans[g] = key.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    //dev::stop_timer("Map construction", timer);
    
    //dev::start_timer("Dictionary lookup", timer);
#if QUANTEDA_USE_TBB
    lookup_mt lookup_mt(texts, spans, id_max, overlap, nomatch, map_keys);
    parallelFor(0, texts.size(), lookup_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        texts[h] = lookup(texts[h], spans, id_max, overlap, nomatch, map_keys);
    }
#endif
    //dev::stop_timer("Dictionary lookup", timer);
    return recompile(texts, types, false, false, is_encoded(types_));
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- list(1:10, c(5, 6) , 4)
#keys <- rep(2, length(dict))
keys <- seq_along(dict) + 1
#qatd_cpp_tokens_lookup(toks, letters, dict, integer(0), 0)
qatd_cpp_tokens_lookup(toks, letters, dict, keys, FALSE, 0)
qatd_cpp_tokens_lookup(toks, letters, dict, keys, TRUE, 0)
qatd_cpp_tokens_lookup(toks, letters, dict, keys, FALSE, 0)
qatd_cpp_tokens_lookup(toks, letters, dict, keys, FALSE, 1)
qatd_cpp_tokens_lookup(toks, letters, dict, keys, FALSE, 2)

*/
