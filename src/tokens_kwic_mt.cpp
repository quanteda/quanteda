#include "quanteda.h"
#include "recompile.h"
//#include "dev.h"
using namespace quanteda;

typedef pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

Targets range(Text tokens,
              const std::vector<std::size_t> &spans,
              const SetNgrams &set_words){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    // This part is the same as tokens_detect 
    Text tokens_pos(tokens.size(), 0);
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            bool is_in = set_words.find(ngram) != set_words.end();
            if (is_in) {
                std::fill(tokens_pos.begin() + i, tokens_pos.begin() + i + span, 1);
            }
        }
    }
    
    Targets targets;
    size_t last = tokens_pos.size();
    size_t start, end;
    for (size_t k = 0; k <= tokens_pos.size(); k++) {
        if ((k == 0 || tokens_pos[k - 1] == 0) && tokens_pos[k] == 1) {
            start = k;
            //Rcout << "starts " << start << "\n";
        }
        if (tokens_pos[k - 1] == 1 && (k == last || tokens_pos[k] == 0)) {
            end = k - 1;
            //Rcout << "ends " << end << "\n";
            targets.push_back(make_pair(start, end));
        }
    }
    return targets;
}

struct range_mt : public Worker{
    
    Texts &input;
    std::vector<Targets> &temp;
    const std::vector<std::size_t> &spans;
    const SetNgrams &set_words;
    
    // Constructor
    range_mt(Texts &input_, std::vector<Targets> &temp_,
             const std::vector<std::size_t> &spans_, const SetNgrams &set_words_):
        input(input_), temp(temp_), spans(spans_), set_words(set_words_){}
    
    // parallelFor calles this function with size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        for (std::size_t h = begin; h < end; h++){
            temp[h] = range(input[h], spans, set_words);
        }
    }
};


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
    
    // Get total number including of sub-elements
    std::size_t len = 0;
    for (std::size_t h = 0; h < output.size(); h++) {
        len += output[h].size();
    }
    
    std::vector<int> pos_from(len), pos_to(len);
    CharacterVector coxs_name_(len), coxs_pre_(len), coxs_target_(len), coxs_post_(len);
    Texts contexts(len);
    std::vector<int>  documents(len);
    
    std::size_t j = 0;
    for (std::size_t h = 0; h < output.size(); h++) {
        Targets targets = output[h];
        if (targets.size() == 0) continue;
        Text tokens = input[h];
        int last = (int)tokens.size() - 1;
        for (size_t i = 0; i < targets.size(); i++) {
            int from = targets[i].first - window;
            int to = targets[i].second + window;
            //Rcout << j << " " << targets[i].first << ":" << targets[i].second << "\n";
            
            // Save as intergers
            Text context(tokens.begin() + std::max(0, from), tokens.begin() + std::min(to, last) + 1);
            contexts[j] = context;
            documents[j] = (int)h + 1;
            
            // Save as strings
            Text cox_pre(tokens.begin() + std::max(0, from), tokens.begin() + targets[i].first);
            Text cox_target(tokens.begin() + targets[i].first, tokens.begin() + targets[i].second + 1);
            Text cox_post(tokens.begin() + targets[i].second + 1, tokens.begin() + std::min(to, last) + 1);
            
            pos_from[j] = targets[i].first + 1;
            pos_to[j] = targets[i].second + 1;
            coxs_pre_[j] = get_text(cox_pre, types_);
            coxs_target_[j] = get_text(cox_target, types_);
            coxs_post_[j] = get_text(cox_post, types_);
            coxs_name_[j] = names_[h];
            j++;
        }
    }
    
    DataFrame output_ = DataFrame::create(_["docname"] = coxs_name_,
                                          _["from"]    = pos_from,
                                          _["to"]      = pos_to,
                                          _["pre"]     = coxs_pre_,
                                          _["keyword"] = coxs_target_,
                                          _["post"]    = coxs_post_,
                                          _["stringsAsFactors"] = false);
    output_.attr("docs") = documents;
    output_.attr("ids") = as<Tokens>(wrap(contexts));
    output_.attr("types") = types_;
    return output_;
}




/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_kwic(toks, letters, list(10), 3)
qatd_cpp_kwic(toks, letters, list(c(3, 4), 7), 2)

*/
