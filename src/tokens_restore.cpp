//#include "dev.h"
#include "lib.h"
#include "recompile.h"
#include "skipgram.h"
using namespace quanteda;

Text join_mark(Text tokens, 
               const MapNgrams &map_marks,
               MapNgrams &map_comps,
               IdNgram &id_comp){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false); // flag matched tokens
    std::size_t match = 0;

    int from = 0;
    int to = 0;
    for (std::size_t i = 0; i < tokens.size(); i++) {
        Ngram ngram(tokens.begin() + i, tokens.begin() + i + 1);
        auto it = map_marks.find(ngram);
        if (it != map_marks.end()) {
            match++;
            flags_match[i] = true;
            if (it->second == 1) {
                from = i;
            } else if (it->second == 2) {
                to = i;
            }
            if (from < to) {
                std::fill(flags_match.begin() + from + 1, flags_match.begin() + to, true); // mark tokens matched
                Ngram tokens_seq(tokens.begin() + from + 1, tokens.begin() + to); // extract tokens between marks
                tokens_multi[i].push_back(ngram_id(tokens_seq, map_comps, id_comp)); // assign ID to ngram
                from = i;
                to = i;
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

struct restore_mt : public Worker{
    
    Texts &texts;
    MapNgrams &map_marks;
    MapNgrams &map_comps;
    IdNgram &id_comp;
    
    // Constructor
    restore_mt(Texts &texts_, 
               MapNgrams &map_marks_, MapNgrams &map_comps_, IdNgram &id_comp_):
               texts(texts_), 
               map_marks(map_marks_), map_comps(map_comps_), id_comp(id_comp_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            texts[h] = join_mark(texts[h], map_marks, map_comps, id_comp);
        }
    }
};

/* 
 * This function substitutes features in tokens object with new IDs. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_restore()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param marks_left_, marks_right_ patterns to mark tokens to restore
 * @param types_ types in the tokens object
 * @param delim_ character to concatenate types
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_restore(const List &texts_, 
                              const List &marks_left_,
                              const List &marks_right_,
                              const CharacterVector &types_,
                              const String &delim_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    std::string delim = delim_;

    unsigned int id_last = types.size();
#if QUANTEDA_USE_TBB
    IdNgram id_comp(id_last + 1);
#else
    IdNgram id_comp = id_last + 1;
#endif

    MapNgrams map_marks; // for matching
    map_marks.max_load_factor(GLOBAL_PATTERN_MAX_LOAD_FACTOR);
    MapNgrams map_comps; // for ID generation
    map_comps.max_load_factor(GLOBAL_NGRAMS_MAX_LOAD_FACTOR);

    Ngrams marks_left = Rcpp::as<Ngrams>(marks_left_);
    for (size_t g = 0; g < marks_left.size(); g++) {
        Ngram value = marks_left[g];
        unsigned int key = 1; // 1 for left
        map_marks.insert(std::pair<Ngram, unsigned int>(value, key));
    }
    Ngrams marks_right = Rcpp::as<Ngrams>(marks_right_);
    for (size_t g = 0; g < marks_right.size(); g++) {
        Ngram value = marks_right[g];
        unsigned int key = 2; // 2 for right
        map_marks.insert(std::pair<Ngram, unsigned int>(value, key));
    }
     
    // dev::Timer timer;
    // dev::start_timer("Token compound", timer);
#if QUANTEDA_USE_TBB
    restore_mt restore_mt(texts, map_marks, map_comps, id_comp);
    parallelFor(0, texts.size(), restore_mt);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        texts[h] = join_mark(texts[h], map_marks, map_comps, id_comp);
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

toks <- list(rep(1:10, 1))
left <- list(1)
right <- list(4)
types <- letters
#qatd_cpp_tokens_compound(toks, dict, types, "_", FALSE)
out <- quanteda:::qatd_cpp_tokens_restore(toks, left, right, types, "_")
unclass(out)

*/

