#include "quanteda.h"
using namespace quanteda;

typedef pair<size_t, size_t> Target;
typedef std::vector<Target> Targets;

inline Targets range(Text tokens,
                     const std::vector<std::size_t> &spans,
                     const SetNgrams &set_words){
    
    if(tokens.size() == 0) return {}; // return empty vector for empty text
    
    // this part is the same as tokens_detect 
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
