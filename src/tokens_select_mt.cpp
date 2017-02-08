//#include "dev.h"
#include "quanteda.h"
using namespace quanteda;

Text keep_token(Text tokens, 
          const std::vector<std::size_t> &spans,
          const SetNgrams &set_words,
          const bool &padding){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = UINT_MAX; // use upper limit as a filler
    Text tokens_copy(tokens.size(), filler);
    if (padding) {
        std::fill(tokens_copy.begin(), tokens_copy.end(), 0);
    }
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                std::copy(ngram.begin(), ngram.end(), tokens_copy.begin() + i);
            }
        }
    }
    if (!padding) tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), filler), tokens_copy.end());
    return tokens_copy;
}

Text remove_token(Text tokens, 
            const std::vector<std::size_t> &spans,
            const SetNgrams &set_words,
            const bool &padding){

    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = std::numeric_limits<unsigned int>::max(); // use upper limit as a filler
    bool match = false;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                match = true;
                std::fill(tokens.begin() + i, tokens.begin() + i + span, filler);
                if (padding) tokens[i] = 0;
            }
        }
    }
    if (match) tokens.erase(std::remove(tokens.begin(), tokens.end(), filler), tokens.end());
    return tokens;
}

struct select_mt : public Worker{
    
    Texts &input;
    Texts &output;
    const std::vector<std::size_t> &spans;
    const SetNgrams &set_words;
    const int &mode;
    const bool &padding;
    
    // Constructor
    select_mt(Texts &input_, Texts &output_, const std::vector<std::size_t> &spans_, 
              const SetNgrams &set_words_, const int &mode_, const bool &padding_):
              input(input_), output(output_), spans(spans_), set_words(set_words_), mode(mode_), padding(padding_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        if (mode == 1) {
            for (std::size_t h = begin; h < end; h++) {
                output[h] = keep_token(input[h], spans, set_words, padding);
            }
        } else if(mode == 2) {
            for (std::size_t h = begin; h < end; h++) {
                output[h] = remove_token(input[h], spans, set_words, padding);
            }
        } else {
            for (std::size_t h = begin; h < end; h++) {
                output[h] = input[h];
            }
        }
    }
};

/* 
 * This funciton select features in tokens object with multiple threads. 
 * The number of threads is set by RcppParallel::setThreadOptions()
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param texts_ tokens ojbect
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * 
 */

// [[Rcpp::export]]
List qatd_cpp_tokens_select(const List &texts_, 
                            const List &words_,
                            int mode,
                            bool padding){
    
    Texts input = Rcpp::as<Texts>(texts_);
    const List words = words_;

    SetNgrams set_words;
    std::vector<std::size_t> spans(words.size());
    for (unsigned int g = 0; g < words.size(); g++) {
        if (has_na(words[g])) continue;
        Ngram word = words[g];
        set_words.insert(word);
        spans[g] = word.size();
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));

    
    // dev::Timer timer;
    Texts output(input.size());
    // dev::start_timer("Token select", timer);
#if QUANTEDA_USE_TBB
    select_mt select_mt(input, output, spans, set_words, mode, padding);
    parallelFor(0, input.size(), select_mt);
#else
    if (mode == 1) {
        for (std::size_t h = 0; h < input.size(); h++) {
            output[h] = keep_token(input[h], spans, set_words, padding);
        }
    } else if(mode == 2) {
        for (std::size_t h = 0; h < input.size(); h++) {
            output[h] = remove_token(input[h], spans, set_words, padding);
        }
    } else {
        for (std::size_t h = 0; h < input.size(); h++){
            output[h] = input[h];
        }
    }
#endif
    // dev::stop_timer("Token select", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    return texts_list;
}

/***R

toks <- list(rep(1:10, 10000), rep(5:15, 10000))
#dict <- as.list(1:100000)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
qatd_cpp_tokens_select(toks, dict, 1, TRUE)




*/
