//#include "dev.h"
#include "lib.h"
#include "recompile.h"
using namespace quanteda;

#if QUANTEDA_USE_TBB
Mutex id_mutex;
#endif

int adjust_window(Text &tokens, int current, int end) {
    int i = current; 
    if (end < current) {
        while (i - 1 >= 0 && i - 1 >= end && tokens[i - 1] != 0) i--;
    } else {
        while (i + 1 < (int)tokens.size() && i + 1 < end && tokens[i + 1] != 0) i++;
    }
    return(i);
}

Text join_comp(Text tokens, 
               const std::vector<std::size_t> &spans,
               const SetNgrams &set_comps,
               MapNgrams &map_comps,
               IdNgram &id_comp,
               const std::pair<int, int> &window){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< bool > flags_link(tokens.size(), false); // flag tokens to join
    std::size_t match = 0;
    
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
    tokens_flat.reserve(tokens.size());
    
    Ngram tokens_seq;
    tokens_seq.reserve(tokens.size());
    
    // Find sequence of matches
    std::size_t len = flags_link.size();
    for (std::size_t i = 0; i < len; i++) {
        //Rcout << "Flag "<< i << ":" << flags_link[i] << "\n";
        if (flags_link[i]) {
                tokens_seq.push_back(tokens[i]);
        } else {
            if (tokens_seq.size() > 0) {
                tokens_seq.push_back(tokens[i]);
#if QUANTEDA_USE_TBB
                id_mutex.lock();
#endif
                UintParam &id = map_comps[tokens_seq];
                if (!id) id = ++id_comp; // assign new ID if not exisis
                //Rcout << "Compund "<< id << ": ";
                //dev::print_ngram(tokens_seq);
                tokens_flat.push_back(id);
#if QUANTEDA_USE_TBB
                id_mutex.unlock();
#endif
                tokens_seq.clear();
            } else {
                tokens_flat.push_back(tokens[i]);
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
                const std::pair<int, int> &window){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false); // flag matched tokens
    std::vector< bool > flags_link(tokens.size(), false); // flag tokens to join
    std::size_t match = 0;

    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            //if (!nested && flags_link[i]) continue; // ignore matched tokens 
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_comps.find(ngram);
            if (it != set_comps.end()) {
                // Adjust window size to exclude padding
                int from = adjust_window(tokens, i, i - window.first);
                int to = adjust_window(tokens, i, i + span + window.second);
                std::fill(flags_match.begin() + from, flags_match.begin() + to + 1, true); // mark tokens matched
                Ngram tokens_seq(tokens.begin() + from, tokens.begin() + to + 1); // extract tokens matched
                
#if QUANTEDA_USE_TBB
                id_mutex.lock();
#endif
                UintParam &id = map_comps[tokens_seq];
                if (!id) id = ++id_comp; // assign new ID if not exists
                //Rcout << "Compund "<< id << ": ";
                //dev::print_ngram(tokens_seq);
                tokens_multi[i].push_back(id); // keep multiple IDs in the same position
#if QUANTEDA_USE_TBB
                id_mutex.unlock();
#endif
                match++;
            }
        }
    }
    
    if (match == 0) return tokens; // return original tokens if no match
    
    // Add original tokens that did not match
    for (std::size_t i = 0; i < tokens.size(); i++) {
        if (!flags_match[i]) {
            tokens_multi[i].push_back(tokens[i]);
            match++;
        }
    }

    // Flatten the vector of vector
    Text tokens_flat;
    tokens_flat.reserve(match);
    for (auto &tokens_sub: tokens_multi) {
        if (tokens_sub.size() > 0) {
            tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.begin() + 1);
        }
    }
    return tokens_flat;
}

struct compound_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const bool &join;
    SetNgrams &set_comps;
    MapNgrams &map_comps;
    IdNgram &id_comp;
    const std::pair<int, int> &window;
    
    // Constructor
    compound_mt(Texts &texts_, const std::vector<std::size_t> &spans_, 
                const bool &join_, SetNgrams &set_comps_, MapNgrams &map_comps_, IdNgram &id_comp_,
                const std::pair<int, int> &window_):
                texts(texts_), spans(spans_), 
                join(join_), set_comps(set_comps_), map_comps(map_comps_), id_comp(id_comp_),
                window(window_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            if (join) {
                texts[h] = join_comp(texts[h], spans, set_comps, map_comps, id_comp, window);
            } else {
                texts[h] = match_comp(texts[h], spans, set_comps, map_comps, id_comp, window);
            }
        }
    }
};

/* 
 * This function substitutes features in tokens object with new IDs. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_compound()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param compounds_ list of patterns to substitute
 * @param types_ types in the tokens object
 * @param delim_ character to concatenate types
 * @param join join overlapped features if true
 * @param window_left numbers tokens on the left-hand side of pattern
 * @param window_right numbers tokens on the right-hand side of pattern
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_compound(const List &texts_, 
                              const List &compounds_,
                              const CharacterVector &types_,
                              const String &delim_,
                              const bool &join,
                              int window_left,
                              int window_right){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    std::string delim = delim_;
    std::pair<int, int> window(window_left, window_right);

    unsigned int id_last = types.size();
    #if QUANTEDA_USE_TBB
    IdNgram id_comp(id_last);
    #else
    IdNgram id_comp = id_last;
    #endif
    
    SetNgrams set_comps; // for matching
    set_comps.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    MapNgrams map_comps; // for ID generation
    map_comps.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);

    Ngrams comps = Rcpp::as<Ngrams>(compounds_);
    std::vector<std::size_t> spans(comps.size());
    for (size_t g = 0; g < comps.size(); g++) {
        set_comps.insert(comps[g]);
        spans[g] = comps[g].size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
     
    // dev::Timer timer;
    // dev::start_timer("Token compound", timer);
#if QUANTEDA_USE_TBB
    compound_mt compound_mt(texts, spans, join, set_comps, map_comps, id_comp, window);
    parallelFor(0, texts.size(), compound_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        if (join) {
            texts[h] = join_comp(texts[h], spans, set_comps, map_comps, id_comp, window);
        } else {
            texts[h] = match_comp(texts[h], spans, set_comps, map_comps, id_comp, window);
        }
    }
#endif
    
    // Extract only keys in order of the ID
    VecNgrams ids_comp(id_comp - id_last);
    for (std::pair<Ngram, unsigned int> it : map_comps) {
        ids_comp[it.second - id_last - 1] = it.first;
    }
    
    // Create compound types
    Types types_comp(ids_comp.size());
    for (std::size_t i = 0; i < ids_comp.size(); i++) {
        Ngram key = ids_comp[i];
        if (key.size() == 0) {
            types_comp[i] = "";
        } else {
            std::string type_ngram = types[key[0] - 1];
            for (std::size_t j = 1; j < key.size(); j++) {
                type_ngram += delim + types[key[j] - 1];
            }
            types_comp[i] = type_ngram;
        }
    }
    types.insert(types.end(), types_comp.begin(), types_comp.end());
    
    // dev::stop_timer("Token compound", timer);
    return recompile(texts, types, true, true, is_encoded(delim_) || is_encoded(types_));
}

/***R

toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- list(c(1, 2), c(3, 4))
#dict <- list(c(1, 2), c(2, 3, 4))
#dict <- list(c(1, 2), c(2, 3), c(4, 5))
dict <- list(6, 4)
types <- letters[seq_along(unique(unlist(toks)))]
#qatd_cpp_tokens_compound(toks, dict, types, "_", FALSE)
qatd_cpp_tokens_compound(toks, dict, types, "_", TRUE, 0, 0)
qatd_cpp_tokens_compound(toks, dict, types, "_", TRUE, 1, 1)
qatd_cpp_tokens_compound(toks, dict, types, "_", FALSE, 1, 1)



*/

