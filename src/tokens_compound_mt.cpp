#include <Rcpp.h>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;

tbb::spin_mutex id_mutex;

Text join_comp(Text tokens, 
               const std::vector<std::size_t> &spans,
               MapNgrams &map_comps,
               IdNgram &id_comp){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< bool > flags_link(tokens.size(), false); // flag correspond to the bourndaries
    std::size_t count_match = 0;
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_comps.find(ngram);
            if (it != map_comps.end()) {
                //Rcout << it->second << "\n";
                std::fill(flags_link.begin() + i, flags_link.begin() + i + span - 1, true); // mark tokens linked
                count_match++;
            }
        }
    }
    
    if (count_match == 0) return tokens; // return original tokens if no match
    
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
                id_mutex.lock();
                unsigned int &id = map_comps[tokens_seq];
                if (!id) id = ++id_comp; // assign new ID if not exisits
                //Rcout << "Compund "<< id << ": ";
                //dev::print_ngram(tokens_seq);
                tokens_flat.push_back(id);
                id_mutex.unlock();
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
                const bool &overlap,
                const MapNgrams &map_comps){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    std::vector< std::vector<unsigned int> > tokens_multi(tokens.size()); 
    std::vector< bool > flags_match(tokens.size(), false);
    std::size_t count_match = 0;
    
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            if (!overlap && flags_match[i]) continue; // ignore matched tokens 
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = map_comps.find(ngram);
            if (it != map_comps.end()) {
                //Rcout << it->second << "\n";
                std::fill(flags_match.begin() + i, flags_match.begin() + i + span, true); // mark tokens matched
                tokens_multi[i].push_back(it->second); // keep multiple keys in the same position
                count_match++;
            }
        }
    }
    
    if (count_match == 0) return tokens; // return original tokens if no match
    
    // Add original tokens that did not match
    for (std::size_t i = 0; i < tokens.size(); i++) {
        if (!flags_match[i]) {
            tokens_multi[i].push_back(tokens[i]); 
            count_match++;
        }
    }
    
    // Flatten the vector of vector
    Text tokens_flat;
    tokens_flat.reserve(count_match);
    for (auto &tokens_sub: tokens_multi) {
        if (overlap) {
            tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.end());
        } else {
            if (tokens_sub.size() > 0) {
                tokens_flat.insert(tokens_flat.end(), tokens_sub.begin(), tokens_sub.begin() + 1);
            }
        }
    }
    return tokens_flat;
}

struct compound_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<std::size_t> &spans;
    const bool &join;
    MapNgrams &map_comps;
    IdNgram &id_comp;
    
    // Constructor
    compound_mt(Texts &input_, Texts &output_, const std::vector<std::size_t> &spans_, 
                const bool &join_, MapNgrams &map_comps_, IdNgram &id_comp_):
                input(input_), output(output_), spans(spans_), join(join_), map_comps(map_comps_), id_comp(id_comp_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++) {
            if (join) {
                output[h] = join_comp(input[h], spans, map_comps, id_comp);
            } else {
                output[h] = match_comp(input[h], spans, true, map_comps);
            }
        }
    }
};

/* 
 * This funciton substitutes features in tokens object with new IDs. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_compound()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param comps_ list of features to substitute
 * @param ids_ IDs to be placed after substitution
 * @param join join overlapped features if true
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_compound(const List &texts_, 
                              const List &comps_,
                              const CharacterVector &types_,
                              const String &delim_,
                              const bool &join){
    
    Texts input = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    std::string delim = delim_;
    const List comps = comps_;
    
    unsigned int id_last = types.size();
    #if RCPP_PARALLEL_USE_TBB && GCC_VERSION >= 40801 // gcc 4.8.1
    IdNgram id_comp(id_last);
    #else
    IdNgram id_comp = id_last;
    #endif

    MapNgrams map_comps;
    std::vector<std::size_t> spans(comps.size());
    for (unsigned int g = 0; g < comps.size(); g++) {
        if (has_na(comps[g])) continue;
        Ngram comp = comps[g];
        map_comps[comp] = ++id_comp;
        spans[g] = comp.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Token compound", timer);
    #if RCPP_PARALLEL_USE_TBB && GCC_VERSION >= 40801 // gcc 4.8.1
    compound_mt compound_mt(input, output, spans, join, map_comps, id_comp);
    parallelFor(0, input.size(), compound_mt);
    #else
    for (std::size_t h = 0; h < input.size(); h++) {
        if (join) {
            output[h] = join_comp(input[h], spans, map_comps, id_comp);
        } else {
            output[h] = match_comp(input[h], spans, true, map_comps);
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
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    texts_list.attr("types") = types;
    return texts_list;
}

/***R

#toks <- list(rep(1:10, 1), rep(5:15, 1))
toks <- list(1:5)
#dict <- list(c(1, 2), c(3, 4))
dict <- list(c(1, 2), c(1, 2, 3))
#dict <- list(c(1, 2), c(2, 3), c(4, 5))
types <- letters[1:length(unique(unlist(toks)))]
#qatd_cpp_tokens_compound(toks, dict, types, "_", FALSE)
qatd_cpp_tokens_compound(toks, dict, types, "_", TRUE)



*/

