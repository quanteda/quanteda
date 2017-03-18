#include "quanteda.h"
#include "recompile.h"
#include "range.h"
//#include "dev.h"
using namespace quanteda;

/* 
 * This funciton finds features extract contexts of features. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used contexts()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param types_ types
 * @param words_ list of target features
 * @param window number of words from target features
 * @param remove if TRUE, remove pad targets
 * @param overlap if TRUE, split overlapping contexts
 */


// [[Rcpp::export]]
List qatd_cpp_tokens_contexts(const List &texts_,
                              const CharacterVector types_,
                              const List &words_,
                              unsigned int window,
                              bool remove,
                              bool overlap){
    
    Texts input = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");

    SetNgrams set_words;
    std::vector<std::size_t> spans = register_ngrams(words_, set_words);
    
    // dev::Timer timer;
    std::vector<Targets> temp(input.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
    range_mt range_mt(input, temp, spans, set_words);
    parallelFor(0, input.size(), range_mt);
#else
    for (std::size_t h = 0; h < input.size(); h++) {
        temp[h] = range(input[h], spans, set_words);
    }
#endif
    
    // get total number including of sub-elements
    std::size_t len = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        len += temp[h].size();
    }
    
    Texts output;
    output.reserve(len);
    std::vector<string> names_new;
    names_new.reserve(len);
    std::vector<int> index_org;
    index_org.reserve(len);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < temp.size(); h++) {
        Targets targets = temp[h];
        if (targets.size() == 0) continue;
        string name = as<string>(names_[h]);
        Text tokens = input[h];
        int last = (int)tokens.size() - 1;
        
        // fill target with padding
        if (remove) {
            for (size_t i = 0; i < targets.size(); i++) {
                std::fill(tokens.begin() + targets[i].first, tokens.begin() + targets[i].second + 1, 0);
            }
        }
        
        int k = 1;
        if (overlap) {
            // extract contexts spliting overlaping
            for (size_t i = 0; i < targets.size(); i++) {
                int from = targets[i].first - window;
                int to = targets[i].second + window;
                //Rcout << k << " in " << h << " " << from << ":" << to << "\n";
                Text context(tokens.begin() + std::max(0, from), tokens.begin() + std::min(to, last) + 1);
                output.push_back(context);
                names_new.push_back(name + ":" + std::to_string(k++));
                index_org.push_back((int)h + 1);
            }
        } else {
            // extract contexts joining overlaping
            for (size_t i = 0; i < targets.size(); i++) {
                for (size_t j = i; j < targets.size(); j++) {
                    if (targets[j].second + window < targets[j + 1].first - window || j + 1 >= targets.size()) {
                        int from = targets[i].first - window;
                        int to = targets[j].second + window;
                        //Rcout << k << " in " << h << " " << from << ":" << to << "\n";
                        Text context(tokens.begin() + std::max(0, from), tokens.begin() + std::min(to, last) + 1);
                        output.push_back(context);
                        names_new.push_back(name + ":" + std::to_string(k++));
                        index_org.push_back((int)h + 1);
                        i = j;
                        break;
                    }
                }
            }
        }
    }
    
    // dev::stop_timer("Dictionary detect", timer);
    List output_ = recompile(output, types);
    output_.attr("names") = encode(names_new);
    output_.attr("index") = index_org;
    return output_;
}



/***R

toks <- list(text1=1:10, text2=5:15, text3=100:110)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
qatd_cpp_tokens_contexts(toks, letters, list(5, 7), 2, TRUE, TRUE)
#qatd_cpp_tokens_contexts(toks, letters, list(5, 7), 2, TRUE, FALSE)
#qatd_cpp_tokens_contexts(toks, letters, list(c(3, 4), 7), 1, TRUE, TRUE)


*/
