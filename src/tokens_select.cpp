#include "lib.h"
#include "dev.h"
#include "recompile.h"
using namespace quanteda;

typedef std::pair<int, int> Position;
typedef std::vector<Position> Positions;

Text keep_token(Text tokens, 
          const std::vector<std::size_t> &spans,
          const SetNgrams &set_words,
          const bool &padding,
          const std::pair<int, int> &window,
          const std::pair<int, int> &pos,
          const bool &skip){
    
    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    const unsigned int filler = UINT_MAX; // use upper limit as a filler
    
    bool match = false;
    std::size_t start, end;
    if (pos.first == 0) {
        start = 0;
    } else if (pos.first > 0) {
        start = std::min((int)tokens.size(), pos.first - 1);
    } else {
        start = std::max(0, (int)tokens.size() + pos.first);
    }
    if (pos.second == 0) {
        end = 0;
    } else if (pos.second > 0) {
        end = std::min((int)tokens.size(), pos.second);
    } else {
        end = std::max(0, (int)tokens.size() + pos.second + 1);
    }
    Text tokens_copy(tokens.size());
    if (padding) {
        std::fill(tokens_copy.begin(), tokens_copy.end(), 0);
    } else {
        std::fill(tokens_copy.begin(), tokens_copy.end(), filler);
    }
    Ngram ngram;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = start; i < end - (span - 1); i++) {
            if (skip) {
                if (span == 1) {
                    ngram = {tokens[i]};
                } else {
                    ngram = {tokens[i], tokens[i + span - 1]};
                }
            } else {
                ngram = {tokens.begin() + i, tokens.begin() + i + span};
            }
            dev::print_ngram(ngram);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                match = true;
                if (window.first == 0 && window.second == 0) {
                    std::copy(ngram.begin(), ngram.end(), tokens_copy.begin() + i);
                } else {
                    int from = std::max((int)i - window.first, 0);
                    int to = std::min((int)i + (int)span + window.second, (int)tokens.size());
                    std::copy(tokens.begin() + from, tokens.begin() + to, tokens_copy.begin() + from);
                }
            }
        }
    }
    if (match) {
        if (!padding) {
            tokens_copy.erase(std::remove(tokens_copy.begin(), tokens_copy.end(), filler), tokens_copy.end());
        }
    } else {
        if (!padding) {
            tokens_copy = {};
        }
    }
    return tokens_copy;
}

Text remove_token(Text tokens, 
            const std::vector<std::size_t> &spans,
            const SetNgrams &set_words,
            const bool &padding,
            const std::pair<int, int> &window,
            const std::pair<int, int> &pos,
            const bool &skip){

    if (tokens.size() == 0) return {}; // return empty vector for empty text
    
    unsigned int filler = UINT_MAX; // use upper limit as a filler
    Text tokens_copy = tokens;
    bool match = false;
    std::size_t start, end;
    if (pos.first == 0) {
        start = 0;
    } else if (pos.first > 0) {
        start = std::min((int)tokens.size(), pos.first - 1);
    } else {
        start = std::max(0, (int)tokens.size() + pos.first);
    }
    if (pos.second == 0) {
        end = 0;
    } else if (pos.second > 0) {
        end = std::min((int)tokens.size(), pos.second);
    } else {
        end = std::max(0, (int)tokens.size() + pos.second + 1);
    }
    Ngram ngram;
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = start; i < end - (span - 1); i++) {
            if (skip) {
                if (span == 1) {
                    ngram = {tokens[i]};
                } else {
                    ngram = {tokens[i], tokens[i + span - 1]};
                }
            } else {
                ngram = {tokens.begin() + i, tokens.begin() + i + span};
            }
            dev::print_ngram(ngram);
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                match = true;
                if (window.first == 0 && window.second == 0) {
                    if (padding) {
                        std::fill(tokens_copy.begin() + i, tokens_copy.begin() + i + span, 0);
                    } else {
                        std::fill(tokens_copy.begin() + i, tokens_copy.begin() + i + span, filler);
                    }
                } else {
                    int from = std::max((int)i - window.first, 0);
                    int to = std::min((int)i + (int)span + window.second, (int)tokens.size());
                    if (padding) {
                        std::fill(tokens_copy.begin() + from, tokens_copy.begin() + to, 0);
                    } else {
                        std::fill(tokens_copy.begin() + from, tokens_copy.begin() + to, filler);
                    }
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
    const std::pair<int, int> &window;
    const Positions &pos;
    const bool &skip;
    
    // Constructor
    select_mt(Texts &texts_, const std::vector<std::size_t> &spans_, 
              const SetNgrams &set_words_, const int &mode_, const bool &padding_, 
              const std::pair<int, int> &window_, const Positions &pos_,
              const bool &skip_):
              texts(texts_), spans(spans_), set_words(set_words_), mode(mode_), padding(padding_), 
              window(window_), pos(pos_), skip(skip_){}
    
    // parallelFor calles this function with std::size_t
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        if (mode == 1) {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = keep_token(texts[h], spans, set_words, padding, window, pos[h], skip);
            }
        } else if (mode == 2) {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = remove_token(texts[h], spans, set_words, padding, window, pos[h], skip);
            }
        } else {
            for (std::size_t h = begin; h < end; h++) {
                texts[h] = texts[h];
            }
        }
    }
};

/* 
 * This function selects features in tokens object with multiple threads. 
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
                            bool padding,
                            int window_left,
                            int window_right,
                            const IntegerVector pos_from_,
                            const IntegerVector pos_to_,
                            const IntegerVector skips_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    std::pair<int, int> window(window_left, window_right);
    std::vector<std::size_t> skips = Rcpp::as< std::vector<std::size_t> >(skips_);
    
    SetNgrams set_words;
    std::vector<std::size_t> n = register_ngrams(words_, set_words);
    
    bool skip = false;
    std::vector<std::size_t> spans;
    for (std::size_t i = 0; i < skips.size(); i++) {
        if (skips[i] == 0) {
            spans.insert(spans.end(), n.begin(), n.end());
        } else {
            spans.push_back(skips[i] + 2);
            skip = true;
        }
    }
    sort(spans.begin(), spans.end());
    spans.erase(unique(spans.begin(), spans.end()), spans.end());
    std::reverse(std::begin(spans), std::end(spans));
    
    for (std::size_t i = 0; i < spans.size(); i++) {
        Rcout << "span = " << spans[i] << "\n";
    }
    
    if (pos_from_.size() != texts.size())
        throw std::range_error("Invalid pos_from");
    if (pos_to_.size() != texts.size())
        throw std::range_error("Invalid pos_to");
    Positions pos(texts.size());
    for (size_t g = 0; g < texts.size(); g++) {
        pos[g] = std::make_pair(pos_from_[g], pos_to_[g]);
    }
    
    // dev::Timer timer;
    // dev::start_timer("Token select", timer);
#if QUANTEDA_USE_TBB
    select_mt select_mt(texts, spans, set_words, mode, padding, window, pos, skip);
    parallelFor(0, texts.size(), select_mt);
#else
    if (mode == 1) {
        for (std::size_t h = 0; h < texts.size(); h++) {
            texts[h] = keep_token(texts[h], spans, set_words, padding, window, pos[h], skip);
        }
    } else if(mode == 2) {
        for (std::size_t h = 0; h < texts.size(); h++) {
            texts[h] = remove_token(texts[h], spans, set_words, padding, window, pos[h], skip);
        }
    } else {
        for (std::size_t h = 0; h < texts.size(); h++){
            texts[h] = texts[h];
        }
    }
#endif
    // dev::stop_timer("Token select", timer);
    return recompile(texts, types, true, false, is_encoded(types_));
}

/***R
require(quanteda)
toks <- list(rep(1:10, 1))
dict <- list(c(1, 2), c(4, 6), 10, 15, 20)
out1 <- quanteda:::qatd_cpp_tokens_select(toks, letters, dict, 1, TRUE, 0, 0, 1, 100, 0)
unclass(out1)
out2 <- quanteda:::qatd_cpp_tokens_select(toks, letters, dict, 1, TRUE, 0, 0, 1, 100, c(1))
unclass(out2)
out3 <- quanteda:::qatd_cpp_tokens_select(toks, letters, dict, 1, TRUE, 0, 0, 1, 100, c(0, 1))
unclass(out3)
out4 <- quanteda:::qatd_cpp_tokens_select(toks, letters, dict, 2, TRUE, 0, 0, 1, 100, c(0, 1))
unclass(out4)
*/
