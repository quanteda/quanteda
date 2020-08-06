//#include "dev.h"
#include "lib.h"
#include "recompile.h"

using namespace quanteda;

Text lookup(Text tokens, 
            const std::vector<std::size_t> &spans,
            const unsigned int &id_max,
            const int &overlap,
            const int &nomatch,
            const MultiMapNgrams &map_keys){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    // Match flag for each token
    std::vector<bool> flags_match_global(tokens.size(), false);
    
    // Match flag for each token for each key
    std::vector< std::vector<bool> > flags_match(id_max);
    if (overlap == 1 || overlap == 2) {
        std::vector<bool> flags_init(tokens.size(), false);
        for (unsigned int h = 0; h < id_max; h++) {
            flags_match[h] = flags_init;
            //Rcout << "h " << h << "\n";
        }
    }
    
    std::size_t match_count = 0;
    std::vector< std::vector<unsigned int> > keys(tokens.size());
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto range = map_keys.equal_range(ngram);
            bool match = false;
            if (overlap == 1) { // local
                for (auto it = range.first; it != range.second; ++it) {
                    unsigned int id = it->second;
                    std::vector< bool > &flags_match_local = flags_match[id - 1];
                    bool flagged = std::any_of(flags_match_local.begin() + i, flags_match_local.begin() + i + span, [](bool v) { return v; });
                    if (!flagged) {
                        keys[i].push_back(id); // keep multiple keys in the same position
                        std::fill(flags_match_local.begin() + i, flags_match_local.begin() + i + span, true); // for each key
                        match = true;
                        match_count++;
                    }
                }
            } else if (overlap == 2) { // global
                bool flagged = std::all_of(flags_match_global.begin() + i, flags_match_global.begin() + i + span, [](bool v) { return v; });
                if (!flagged) {
                    for (auto it = range.first; it != range.second; ++it) {
                        unsigned int id = it->second;
                        std::vector< bool > &flags_match_local = flags_match[id - 1];
                        bool flagged = std::any_of(flags_match_local.begin() + i, flags_match_local.begin() + i + span, [](bool v) { return v; });
                        if (!flagged) {
                            keys[i].push_back(id); // keep multiple keys in the same position
                            std::fill(flags_match_local.begin() + i, flags_match_local.begin() + i + span, true); // for each key
                            match = true;
                            match_count++;
                        }
                    }
                }
            }
            if (match)
                std::fill(flags_match_global.begin() + i, flags_match_global.begin() + i + span, true); // for all keys
        }
    }
    
    if (match_count == 0) {
        if (nomatch == 0) {
            // return empty vector
            return {}; 
        } else if (nomatch == 1) {
            // return tokens with no-match
            Text keys_flat(tokens.size(), id_max); 
            return keys_flat;
        } else if (nomatch == 2) {
            // return shifted tokens in exclusive mode
            Text keys_flat(tokens.size());
            for (std::size_t i = 0; i < tokens.size(); i++) {
                if (tokens[i] == 0) {
                    keys_flat[i] = 0;
                } else {
                    keys_flat[i] = id_max + tokens[i];
                }
            }
            return keys_flat;
        }
    }
    
    // Flatten the vector of vectors
    Text keys_flat;
    if (nomatch > 0) {
        keys_flat.reserve(match_count + tokens.size());
    } else {
        keys_flat.reserve(match_count);
    }
    for (size_t i = 0; i < keys.size(); i++) {
        if (flags_match_global[i]) {
            std::vector<unsigned int> key_sub = keys[i];
            if (key_sub.size() > 1) {
                std::sort(key_sub.begin(), key_sub.end()); // sort in order of keys
            }
            keys_flat.insert(keys_flat.end(), key_sub.begin(), key_sub.end());
        } else {
            if (nomatch == 1) {
                keys_flat.push_back(id_max); // pad with no-match
            } else if (nomatch == 2) {
                if (tokens[i] == 0) {
                    keys_flat.push_back(0);
                } else {
                    keys_flat.push_back(id_max + tokens[i]); // keep original token
                }
            }
        }
    }
    return keys_flat;
}


struct lookup_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const unsigned int &id_max;
    const int &overlap;
    const int &nomatch;
    const MultiMapNgrams &map_keys;
    
    // Constructor
    lookup_mt(Texts &texts_, const std::vector<std::size_t> &spans_, const unsigned int &id_max_,
              const int &overlap_, const int &nomatch_, const MultiMapNgrams &map_keys_):
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
* This function finds patterns in tokens object. This is similar to tokens_replace, 
* but all overlapping or nested patterns are detected and recorded by IDs.
* The number of threads is set by RcppParallel::setThreadOptions()
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param words_ list of dictionary values
* @param keys_ IDs of dictionary keys
* @param overlap ignore overlapped words: 1=local, 2=global, 3=none
* @param nomatch determine how to treat unmached words: 0=remove, 1=pad; 2=keep
*/


// [[Rcpp::export]]
List qatd_cpp_tokens_lookup(const List &texts_,
                            const CharacterVector types_,
                            const List &words_,
                            const IntegerVector &keys_,
                            const int overlap,
                            const int nomatch){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    unsigned int id_max(0);
    if (nomatch == 2) {
        id_max = keys_.size() > 0 ? Rcpp::max(keys_) : 0;
    } else {
        id_max = types_.size();
    }
    //Rcout << id_max << "\n";
    
    //dev::Timer timer;
    
    //dev::start_timer("Map construction", timer);

    MultiMapNgrams map_keys;
    map_keys.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams words = Rcpp::as<Ngrams>(words_);
    std::vector<unsigned int> keys = Rcpp::as< std::vector<unsigned int> >(keys_);
    
    size_t len = std::min(words.size(), keys.size());
    std::vector<std::size_t> spans(len);
    for (size_t g = 0; g < len; g++) {
        Ngram value = words[g];
        unsigned int key = keys[g];
        map_keys.insert(std::pair<Ngram, unsigned int>(value, key));
        spans[g] = value.size();
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
    if (nomatch == 2) {
        return recompile(texts, types, true, true, is_encoded(types_));
    } else {
        return recompile(texts, types, false, false, is_encoded(types_));
    }
    
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
