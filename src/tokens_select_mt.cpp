#include "quanteda.h"
//#include "dev.h"
#include "recompile.h"
using namespace quanteda;

Text keep_token(Text tokens, 
          const std::vector<std::size_t> &spans,
          const SetNgrams &set_words,
          const bool &padding){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = UINT_MAX; // use upper limit as a filler
    
    bool match = false;
    Text tokens_copy(tokens.size());
    if (padding) {
        std::fill(tokens_copy.begin(), tokens_copy.end(), 0);
    } else {
        std::fill(tokens_copy.begin(), tokens_copy.end(), filler);
    }
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                match = true;
                std::copy(ngram.begin(), ngram.end(), tokens_copy.begin() + i);
            }
        }
    }
    if (match) {
        if (!padding) {
            tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), filler), tokens_copy.end());
        }
    } else {
        tokens_copy = {};
    }
    return tokens_copy;
}

Text remove_token(Text tokens, 
            const std::vector<std::size_t> &spans,
            const SetNgrams &set_words,
            const bool &padding){

    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = UINT_MAX; // use upper limit as a filler
    Text tokens_copy = tokens;
    bool match = false;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = 0; i < tokens.size() - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                match = true;
                if (padding) {
                    std::fill(tokens_copy.begin() + i, tokens_copy.begin() + i + span, 0);
                } else {
                    std::fill(tokens_copy.begin() + i, tokens_copy.begin() + i + span, filler);
                }
            }
        }
    }
    if (match && !padding) {
        tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), filler), tokens_copy.end());
    }
    return tokens_copy;
}

struct select_mt : public Worker{
    
    Texts &texts;
    const std::vector<std::size_t> &spans;
    const SetNgrams &set_words;
    const int &mode;
    const bool &padding;
    
    // Constructor
    select_mt(Texts &texts_, const std::vector<std::size_t> &spans_, 
              const SetNgrams &set_words_, const int &mode_, const bool &padding_):
              texts(texts_), spans(spans_), set_words(set_words_), mode(mode_), padding(padding_) {}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        if (mode == 1) {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = keep_token(texts[h], spans, set_words, padding);
            }
        } else if(mode == 2) {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = remove_token(texts[h], spans, set_words, padding);
            }
        } else {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = texts[h];
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
                            const CharacterVector types_,
                            const List &words_,
                            int mode,
                            bool padding){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    
    SetNgrams set_words;
    std::vector<std::size_t> spans = register_ngrams(words_, set_words);
    
    // dev::Timer timer;
    // dev::start_timer("Token select", timer);
#if QUANTEDA_USE_TBB
    select_mt select_mt(texts, spans, set_words, mode, padding);
    parallelFor(0, texts.size(), select_mt);
#else
    if (mode == 1) {
        for (std::size_t h = 0; h < texts.size(); h++) {
            texts[h] = keep_token(texts[h], spans, set_words, padding);
        }
    } else if(mode == 2) {
        for (std::size_t h = 0; h < texts.size(); h++) {
            texts[h] = remove_token(texts[h], spans, set_words, padding);
        }
    } else {
        for (std::size_t h = 0; h < texts.size(); h++){
            texts[h] = texts[h];
        }
    }
#endif
    // dev::stop_timer("Token select", timer);
    return recompile(texts, types);
}

/***R
toks <- list(rep(1:10, 1))
#toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- as.list(1:100000)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
qatd_cpp_tokens_select(toks, letters, dict, 1, TRUE)




*/
