#include "lib.h"
#include "skipgram.h"
//#include "dev.h"
using namespace quanteda;

int adjust_window(Text &tokens, int begin, int end) {
    int i = begin; 
    if (end < begin) {
        while (i - 1 >= 0 && i - 1 >= end && tokens[i - 1] != 0) i--;
    } else {
        while (i + 1 < (int)tokens.size() && i + 1 < end && tokens[i + 1] != 0) i++;
    }
    return(i);
}

bool is_nested(std::vector<bool> &flags, int begin, int end) {
    begin = std::max(0, begin);
    end = std::min(end, (int)flags.size());
    for (int i = begin; i < end; i++) {
        if (!flags[i])
            return(false);
    }
    return(true);
}

Text join_comp(Text tokens, 
               const std::vector<std::size_t> &spans,
               const SetNgrams &set_comps,
               MapNgrams &map_comps,
               IdNgram &id_comp,
               const bool &keep,
               const std::pair<int, int> &window){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    std::vector< bool > flags_link(tokens.size(), false); // flag tokens to join
    std::size_t match = tokens.size();
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_comps.find(ngram);
            if (it != set_comps.end()) {
                // Adjust window size to exclude padding
                int from = adjust_window(tokens, i, i - window.first);
                int to = adjust_window(tokens, i, i + span + window.second);
                std::fill(flags_link.begin() + from, flags_link.begin() + to, true); // mark tokens linked
                match++;
            }
            flags_link.back() = false; // last value should be false
        }
    }
    
    if (match == 0) return tokens; // return original tokens if no match
    
    Text tokens_flat;
    tokens_flat.reserve(match);
    
    Ngram tokens_seq;
    tokens_seq.reserve(tokens.size());
    
    // Find sequence of matches
    std::size_t len = flags_link.size();
    for (std::size_t i = 0; i < len; i++) {
        //Rcout << "Flag "<< i << ":" << flags_link[i] << "\n";
        if (flags_link[i]) {
            tokens_seq.push_back(tokens[i]);
        } else {
            if (tokens_seq.empty()) {
                tokens_flat.push_back(tokens[i]);
            } else {
                tokens_seq.push_back(tokens[i]);
                if (keep)
                    tokens_flat.insert(tokens_flat.end(), tokens_seq.begin(), tokens_seq.end());
                tokens_flat.push_back(ngram_id(tokens_seq, map_comps, id_comp)); // assign ID to ngram
                tokens_seq.clear();
            }
        }
    }
    
    return tokens_flat;
}

Text match_comp(Text tokens, 
                const std::vector<std::size_t> &spans,
                const SetNgrams &set_comps,
                MapNgrams &map_comps,
                IdNgram &id_comp,
                const bool &keep,
                const std::pair<int, int> &window){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
    std::vector< Text > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false); // flag matched tokens
    std::size_t match = tokens.size();
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_comps.find(ngram);
            if (it != set_comps.end()) {
                if (is_nested(flags_match, i, i + span)) continue; // ignore nested tokens 
                // Adjust window size to exclude padding
                int from = adjust_window(tokens, i, i - window.first);
                int to = adjust_window(tokens, i, i + span + window.second);
                std::fill(flags_match.begin() + from, flags_match.begin() + to + 1, true); // mark tokens matched
                Ngram tokens_seq(tokens.begin() + from, tokens.begin() + to + 1); // extract tokens matched
                tokens_multi[i + span - 1].push_back(ngram_id(tokens_seq, map_comps, id_comp)); // assign ID to ngram
                match++;
            }
        }
    }
    
    if (match == 0) return tokens; // return original tokens if no match
    
    Text tokens_flat;
    tokens_flat.reserve(match);
    
    // Flatten the vector of vector
    for (std::size_t i = 0; i < tokens_multi.size(); i++) {
        Text tokens_sub = tokens_multi[i];
        if (!flags_match[i]) {
            tokens_flat.push_back(tokens[i]);
        } else {
            if (keep)
                tokens_flat.push_back(tokens[i]);
            if (tokens_sub.size())
                tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.end());
        }
    }
    return tokens_flat;
}

/* 
 * Function to compound tokens
 * @used tokens_compound()
 * @creator Kohei Watanabe
 * @param compounds_ list of patterns to substitute
 * @param delim_ character to concatenate types
 * @param join join overlapped features if true
 * @param keep original unigrams if true
 * @param window_left numbers tokens on the left-hand side of pattern
 * @param window_right numbers tokens on the right-hand side of pattern
 * @param bypass_ select documents to modify: TRUE=modify, FALSE=don't modify
 */

// [[Rcpp::export]]
TokensPtr cpp_tokens_compound(TokensPtr xptr,
                              const List &compounds_,
                              const String &delim_,
                              const bool &join,
                              const bool &keep,
                              int window_left,
                              int window_right,
                              const LogicalVector bypass_,
                              const int thread = -1) {
    
    Texts texts = xptr->texts;
    Types types = xptr->types;
    std::string delim = delim_;
    std::pair<int, int> window(window_left, window_right);
    
    if (bypass_.size() != (int)texts.size())
        throw std::range_error("Invalid bypass");
    std::vector<bool> bypass = Rcpp::as< std::vector<bool> >(bypass_);
    
    unsigned int id_last = types.size();
//#if QUANTEDA_USE_TBB
    IdNgram id_comp(id_last + 1);
// #else
//     IdNgram id_comp = id_last + 1;
// #endif

    SetNgrams set_comps; // for matching
    set_comps.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    MapNgrams map_comps; // for ID generation
    map_comps.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);
    
    std::vector<std::size_t> spans = register_ngrams(compounds_, set_comps, true);
    
    // Ngrams comps = Rcpp::as<Ngrams>(compounds_);
    // std::vector<std::size_t> spans(comps.size());
    // for (std::size_t g = 0; g < comps.size(); g++) {
    //     Ngram comp = comps[g];
    //     // ignore patterns with paddings
    //     if (std::find(comp.begin(), comp.end(), 0) == comp.end()) {
    //         set_comps.insert(comp);
    //         spans[g] = comp.size();
    //     }
    // }
    // sort(spans.begin(), spans.end());
    // spans.erase(unique(spans.begin(), spans.end()), spans.end());
    // std::reverse(std::begin(spans), std::end(spans));
     
    // dev::Timer timer;
    // dev::start_timer("Token compound", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                if (bypass[h])
                    continue;
                if (join) {
                    texts[h] = join_comp(texts[h], spans, set_comps, map_comps, id_comp, 
                                         keep, window);
                } else {
                    texts[h] = match_comp(texts[h], spans, set_comps, map_comps, id_comp, 
                                          keep, window);
                }
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        if (bypass[h])
            continue;
        if (join) {
            texts[h] = join_comp(texts[h], spans, set_comps, map_comps, id_comp, keep, window);
        } else {
            texts[h] = match_comp(texts[h], spans, set_comps, map_comps, id_comp, keep, window);
        }
    }
#endif

    // Extract only keys in order of the ID
    VecNgrams ids_comp(id_comp - id_last - 1);
    for (std::pair<Ngram, unsigned int> it : map_comps) {
        // Rcout << "Ngram ";
        // dev::print_ngram(it.first);
        // Rcout << "ID " << it.second << "\n";
        ids_comp[it.second - id_last - 1] = it.first;
    }
    
    // Create compound types
    Types types_comp(ids_comp.size());
    for (std::size_t i = 0; i < ids_comp.size(); i++) {
        types_comp[i] = join_strings(ids_comp[i], types, delim);
    }
    types.insert(types.end(), types_comp.begin(), types_comp.end());
    
    // dev::stop_timer("Token compound", timer);
    xptr->texts = texts;
    xptr->types = types;
    xptr->recompiled = false;
    return xptr;
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- list(c(1, 2), c(3, 4))
#dict <- list(c(1, 2), c(2, 3, 4))
#dict <- list(c(1, 2), c(2, 3), c(4, 5))
dict <- list(6, 4)
types <- letters[seq_along(unique(unlist(toks)))]
#cpp_tokens_compound(toks, dict, types, "_", FALSE)
cpp_tokens_compound(toks, dict, types, "_", TRUE, 0, 0)
cpp_tokens_compound(toks, dict, types, "_", TRUE, 1, 1)
cpp_tokens_compound(toks, dict, types, "_", FALSE, 1, 1)



*/

