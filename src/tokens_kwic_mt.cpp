#include "quanteda.h"
#include "recompile.h"
#include "range.h"
//#include "dev.h"
using namespace quanteda;

/* 
 * This funciton generate generates keyword-in-contexts. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used kwic()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param words_ list of target features
 * 
 */

// [[Rcpp::export]]
DataFrame qatd_cpp_kwic(const List &texts_,
                        const CharacterVector types_,
                        const List &words_,
                        unsigned int window){
    
    Texts input = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");
    
    SetNgrams set_words;
    std::vector<std::size_t> spans = register_ngrams(words_, set_words);
    
    // dev::Timer timer;
    std::vector<Targets> output(input.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
    range_mt range_mt(input, output, spans, set_words);
    parallelFor(0, input.size(), range_mt);
#else
    for (std::size_t h = 0; h < input.size(); h++) {
        output[h] = range(input[h], spans, set_words);
    }
#endif
    
    // get total number including of sub-elements
    std::size_t len = 0;
    for (std::size_t h = 0; h < output.size(); h++) {
        len += output[h].size();
    }
    std::vector<int> pos(len);
    CharacterVector coxs_name_(len), coxs_pre_(len), coxs_target_(len), coxs_post_(len);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < output.size(); h++) {
        Text tokens = input[h];
        Targets targets = output[h];
        int last = (int)tokens.size() - 1;
        for (size_t i = 0; i < targets.size(); i++) {
            int from = targets[i].first - window;
            int to = targets[i].second + window;
            
            //Rcout << j << " " << from << ":" << to << "\n";
            
            // extract contexts
            Text cox_pre(tokens.begin() + std::max(0, from), tokens.begin() + targets[i].first + 1);
            Text cox_target(tokens.begin() + targets[i].first, tokens.begin() + targets[i].second + 1);
            Text cox_post(tokens.begin() + targets[i].second + 1, tokens.begin() + std::min(to, last) + 1);
            
            pos[j] = targets[i].first;
            coxs_pre_[j] = get_text(cox_pre, types_);
            coxs_target_[j] = get_text(cox_target, types_);
            coxs_post_[j] = get_text(cox_post, types_);
            coxs_name_[j] = names_[h];
            j++;
        }
    }
    
    DataFrame output_ = DataFrame::create(_["document"] = coxs_name_,
                                          _["position"] = pos,
                                          _["pre"]      = coxs_pre_,
                                          _["target"]   = coxs_target_,
                                          _["post"]     = coxs_post_,
                                          _["stringsAsFactors"] = false);
    return output_;
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_kwic(toks, letters, list(c(3, 4), 7), 3)
qatd_cpp_kwic(toks, letters, list(c(3, 4), 7), 3)

*/
