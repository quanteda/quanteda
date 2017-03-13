#include "quanteda.h"
#include "recompile.h"
#include "dev.h"
using namespace quanteda;

typedef pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

Targets range(Text tokens,
              const std::vector<std::size_t> &spans,
              const SetNgrams &set_words){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
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
    size_t len = tokens_pos.size();
    size_t start, end;
    for (size_t k = 0; k < tokens_pos.size(); k++) {
        if ((k == 0 || tokens_pos[k - 1] == 0) && tokens_pos[k] == 1) {
            start = k;
            //Rcout << "starts " << start << "\n";
        }
        if (tokens_pos[k - 1] == 1 && (k == len - 1 || tokens_pos[k] == 0)) {
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
 * This funciton finds features in tokens object. This is similar to tokens_tookup, 
 * but returns a tokens object filled with zero or one.
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used kwic()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to find
 * 
 */


// [[Rcpp::export]]
List qatd_cpp_tokens_contexts(const List &texts_,
                              const CharacterVector types_,
                              const List &words_,
                              unsigned int window){
    
    Texts input = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);
    CharacterVector names_ = texts_.attr("names");

    SetNgrams set_words;
    std::vector<std::size_t> spans(words_.size());
    for (unsigned int g = 0; g < (unsigned int)words_.size(); g++) {
        if (has_na(words_[g])) continue;
        Ngram word = words_[g];
        set_words.insert(word);
        spans[g] = word.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
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
    
    // separate contexts
    Texts output;
    std::vector<std::string> names;
    names.reserve(input.size());
    std::vector<int> positions;
    positions.reserve(input.size());
    
    for (std::size_t h = 0; h < temp.size(); h++) {
        string name = Rcpp::as<string>(names_[h]);
        Text tokens = input[h];
        Targets targets = temp[h];
        int last = (int)tokens.size();
        for (size_t l = 0; l < targets.size(); l++) {
            int from = targets[l].first - window;
            int to = targets[l].second + window;
            
            //Rcout << targets[l].first << ":" << targets[l].second << "\n";
            //Rcout << from << ":" << to << "\n";
            
            Text context(tokens.begin() + std::max(0, from), 
                         tokens.begin() + std::min(to, last));
            output.push_back(context);
            names.push_back(name + ":" + std::to_string(l + 1));
            positions.push_back(targets[l].first + 1); // for kwic
        }
    }
    
    // dev::stop_timer("Dictionary detect", timer);
    List output_ = recompile(output, types);
    output_.attr("names") = names;
    output_.attr("positions") = positions;
    return output_;
}

/***R

toks <- list(text1=1:10, text2=5:15)
#toks <- rep(list(rep(1:10, 1), rep(5:15, 1)), 1)
#dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#qatd_cpp_tokens_contexts(toks, dict, 2)
qatd_cpp_tokens_contexts(toks, letters, list(c(3, 4), 7), 3)

*/
