#include "lib.h"
//#include "dev.h"
using namespace quanteda;

typedef std::pair<int, int> Position;
typedef std::vector<Position> Positions;

Text keep_token(Text tokens, 
          const std::vector<std::size_t> &spans,
          const SetNgrams &set_words,
          const bool &padding,
          const std::pair<int, int> &window,
          const std::pair<int, int> &pos){
    
    if (tokens.empty()) return {}; // return empty vector for empty text
    
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
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = start; i < end - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
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
            const std::pair<int, int> &pos){

    if (tokens.empty()) return {}; // return empty vector for empty text
    
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
    for (std::size_t span : spans) { // substitution starts from the longest sequences
        if (tokens.size() < span) continue;
        for (std::size_t i = start; i < end - (span - 1); i++) {
            Ngram ngram(tokens.begin() + i, tokens.begin() + i + span);
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

/* 
 * Function to selects tokens
 * @used tokens_select()
 * @creator Kohei Watanabe
 * @param words_ list of features to remove or keep 
 * @param mode_ 1: keep; 2: remove
 * @param padding_ fill places where features are removed with zero
 * @param bypass_ select documents to modify: TRUE=modify, FALSE=don't modify
 */


// [[Rcpp::export]]
TokensPtr cpp_tokens_select(TokensPtr xptr,
                                 const List &words_,
                                 int mode,
                                 bool padding,
                                 int window_left,
                                 int window_right,
                                 const IntegerVector pos_from_,
                                 const IntegerVector pos_to_,
                                 const LogicalVector bypass_,
                                 const int thread = -1) {

    Texts texts = xptr->texts;
    std::pair<int, int> window(window_left, window_right);
    
    SetNgrams set_words;
    std::vector<std::size_t> spans = register_ngrams(words_, set_words);
    
    if (pos_from_.size() != (int)texts.size())
        throw std::range_error("Invalid pos_from");
    if (pos_to_.size() != (int)texts.size())
        throw std::range_error("Invalid pos_to");
    Positions pos(texts.size());
    for (size_t g = 0; g < texts.size(); g++) {
        pos[g] = std::make_pair(pos_from_[g], pos_to_[g]);
    }
    
    if (bypass_.size() != (int)texts.size())
        throw std::range_error("Invalid bypass");
    std::vector<bool> bypass = Rcpp::as< std::vector<bool> >(bypass_);
    
    // dev::Timer timer;
    // dev::start_timer("Token select", timer);
    std::size_t H = texts.size();
#if QUANTEDA_USE_TBB
    tbb::task_arena arena(thread);
    arena.execute([&]{
        tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
            for (int h = r.begin(); h < r.end(); ++h) {
                if (bypass[h])
                    continue;
                if (mode == 1) {
                    texts[h] = keep_token(texts[h], spans, set_words, padding, window, pos[h]);
                } else if(mode == 2) {
                    texts[h] = remove_token(texts[h], spans, set_words, padding, window, pos[h]);
                }
            }    
        });
    });
#else
    for (std::size_t h = 0; h < H; h++) {
        if (bypass[h])
            continue;
        if (mode == 1) {
            texts[h] = keep_token(texts[h], spans, set_words, padding, window, pos[h]);
        } else if(mode == 2) {
            texts[h] = remove_token(texts[h], spans, set_words, padding, window, pos[h]);
        }
    }
#endif
    // dev::stop_timer("Token select", timer);
    xptr->texts = texts;
    xptr->recompiled = false;
    xptr->padded = padding;
    return xptr;
}


/***R
toks <- list(rep(1:10, 1))
#toks <- list(rep(1:10, 1), rep(5:15, 1))
#dict <- as.list(1:100000)
dict <- list(c(1, 2), c(5, 6), 10, 15, 20)
#dict <- list(c(99))
#cpp_tokens_select(toks, letters, dict, 1, TRUE, 1, 1)
cpp_tokens_select(toks, letters, dict, 1, TRUE, 0, 0, 1, 100)
cpp_tokens_select(toks, letters, dict, 1, TRUE, 0, 1, 1, 100)
cpp_tokens_select(toks, letters, dict, 1, FALSE, 0, 0, 1, 2)
cpp_tokens_select(toks, letters, dict, 2, TRUE, 0, 0, 1, 5)
cpp_tokens_select(toks, letters, dict, 2, TRUE, 0, 1, 1, 2)
cpp_tokens_select(toks, letters, dict, 2, TRUE, 0, 2, 1, 3)
cpp_tokens_select(toks, letters, as.list(1:10), 1, FALSE, 0, 0, 1, 2)
cpp_tokens_select(toks, letters, as.list(1:10), 1, TRUE, 0, 0, 1, 2)
cpp_tokens_select(toks, letters, as.list(1:10), 2, FALSE, 0, 0, 1, 1)
cpp_tokens_select(toks, letters, as.list(1:10), 2, TRUE, 0, 0, 1, 1)

*/
