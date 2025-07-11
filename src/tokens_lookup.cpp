#include "lib.h"
//#include "dev.h"

using namespace quanteda;

Text lookup(Text tokens, 
            const std::vector<std::size_t> &spans,
            const unsigned int &id_max,
            const int &overlap,
            const int &mode,
            const MultiMapNgrams &map_keys,
            const bool &bypass){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
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
    if (!bypass) {
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
                            if (mode == 3) {
                                keys[i + span - 1].push_back(id); // keep multiple keys in the same position
                            } else {
                                keys[i].push_back(id); // keep multiple keys in the same position
                            }
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
                                if (mode == 3) {
                                    keys[i + span - 1].push_back(id); // keep multiple keys in the same position
                                } else {
                                    keys[i].push_back(id); // keep multiple keys in the same position
                                }
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
    }
    
    if (match_count == 0) {
        if (mode == 0) {
            // return empty vector
            return {}; 
        } else if (mode == 1) {
            // return tokens with no-match
            Text keys_flat(tokens.size(), id_max); 
            return keys_flat;
        } else if (mode == 2 || mode == 3) {
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
    if (mode > 0) {
        keys_flat.reserve(match_count + tokens.size());
    } else {
        keys_flat.reserve(match_count);
    }
    for (size_t i = 0; i < keys.size(); i++) {
        if (mode == 3) { // keep all tokens
            if (tokens[i] == 0) {
                keys_flat.push_back(0);
            } else {
                keys_flat.push_back(id_max + tokens[i]); // keep original token always
            }
        }
        if (flags_match_global[i]) {
            std::vector<unsigned int> key_sub = keys[i];
            if (key_sub.size() > 1) {
                std::sort(key_sub.begin(), key_sub.end()); // sort in order of keys
            }
            keys_flat.insert(keys_flat.end(), key_sub.begin(), key_sub.end());
        } else {
            if (mode == 1) {
                keys_flat.push_back(id_max); // pad unmatched tokens
            } else if (mode == 2) { // keep unmatched tokens
                if (tokens[i] == 0) {
                    keys_flat.push_back(0);
                } else {
                    keys_flat.push_back(id_max + tokens[i]); 
                }
            }
        }
    }
    return keys_flat;
}

/* 
* Function to find dictionary keywords
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param words_ list of dictionary values
* @param keys_ IDs of dictionary keys
* @param overlap ignore overlapped words: 1=local, 2=global, 3=none
* @param mode determine handling of original tokens: 
*   0=remove unmached, 1=pad unmached with 'nomatch'; 2=keep unmached; 3=keep all
* @param bypass_ select documents to modify: TRUE=modify, FALSE=don't modify
*/

// [[Rcpp::export]]
TokensPtr cpp_tokens_lookup(TokensPtr xptr,
                                 const List &words_,
                                 const IntegerVector &keys_,
                                 const CharacterVector &types_,
                                 const int overlap,
                                 const int mode,
                                 const LogicalVector bypass_,
                                 const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = Rcpp::as<Types>(types_);
    
    if (words_.size() != keys_.size())
        throw std::range_error("Invalid words and keys");
    
    if (bypass_.size() != (int)texts.size())
        throw std::range_error("Invalid bypass");
    std::vector<bool> bypass = Rcpp::as< std::vector<bool> >(bypass_);
    
    std::vector<unsigned int> keys = Rcpp::as< std::vector<unsigned int> >(keys_);
    unsigned int id_max = 0;
    if (mode == 2 || mode == 3) {
        types.insert(types.end(), xptr->types.begin(), xptr->types.end());
        if (keys_.size() > 0)
            id_max = *max_element(keys.begin(), keys.end());
    } else {
        id_max = types.size();
    }
    MultiMapNgrams map_keys;
    map_keys.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    Ngrams words = to_ngrams(words_);
    
    size_t G = words.size();
    std::vector<std::size_t> spans(G);
    for (std::size_t g = 0; g < G; g++) {
        Ngram value = words[g];
        unsigned int key = keys[g];
        map_keys.insert(std::make_pair(value, key));
        spans[g] = value.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    //dev::stop_timer("Map construction", timer);
    
    //dev::start_timer("Dictionary lookup", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                texts[h] = lookup(texts[h], spans, id_max, overlap, mode, map_keys, bypass[h]);
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        texts[h] = lookup(texts[h], spans, id_max, overlap, mode, map_keys, bypass[h]);
    }
#endif
    
    xptr->texts = texts;
    xptr->types = types;
    
    if (mode == 0 || mode == 1) { // exclusive mode
        // NOTE: values might need to be reset
        xptr->recompiled = true;
    } else {
        xptr->recompiled = false;
    }
    return xptr;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- list(1:10, c(5, 6) , 4)
#keys <- rep(2, length(dict))
keys <- seq_along(dict) + 1
#cpp_tokens_lookup(toks, letters, dict, integer(0), 0)
cpp_tokens_lookup(toks, dict, keys, FALSE, 0)
cpp_tokens_lookup(toks, dict, keys, TRUE, 0)
cpp_tokens_lookup(toks, dict, keys, FALSE, 0)
cpp_tokens_lookup(toks, dict, keys, FALSE, 1)
cpp_tokens_lookup(toks, dict, keys, FALSE, 2)

*/
