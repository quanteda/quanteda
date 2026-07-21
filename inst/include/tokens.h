//#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "dev.h"
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

typedef std::vector<unsigned int> Text;
typedef std::vector<Text> Texts;
typedef std::string Type;
typedef std::vector<Type> Types;
typedef std::vector<unsigned int> Ids;

class TokensObj {
    public:
        TokensObj(bool recompiled_ = false, bool padded_ = false): 
                  recompiled(recompiled_), padded(padded_) {}
        
        TokensObj(Texts texts_, Types types_, 
                  bool recompiled_ = false, bool padded_ = false): 
                  texts(texts_), types(types_), 
                  recompiled(recompiled_), padded(padded_) {}
        
        // variables
        Texts texts;
        Types types;
        bool recompiled;
        bool padded;
        
        // functions
        void recompile();

    private:
        bool is_duplicated(Types types); // NOTE: consider removing
};

inline bool TokensObj::is_duplicated(Types types) {
    std::unordered_set<std::string> set;
    for (std::size_t i = 0; i < types.size() - 1; i++) {
        auto it = set.insert(types[i]);
        if (types[i] == "" || !it.second)
            return true;
    }
    return false;
}

inline void TokensObj::recompile() {

    if (recompiled) return; // do nothing
    
    const std::size_t G = types.size();
    const std::size_t H = texts.size();
    unsigned int id_limit = G;
    unsigned int id_unused = id_limit + 1;

    // Re-index tokens -------------------
    
    unsigned int id = 1;
    std::vector<unsigned int> ids(G, id_unused);
    
    for (std::size_t g = 0; g < G; g++) {
        if (types[g] == "") 
            ids[g] = 0; // padding
    }
    int count_pad = 0;
    for (std::size_t h = 0; h < H; h++) {
        Text &text_new = texts[h];
        std::size_t I = texts[h].size();
        for (std::size_t i = 0; i < I; i++) {
            if (texts[h][i] > id_limit) 
                throw std::range_error("Invalid tokens object");
            if (texts[h][i] == 0) {
                text_new[i] = 0;
            } else {
                if (ids[texts[h][i] - 1] == id_unused) {
                    ids[texts[h][i] - 1] = id;
                    id++;
                }
                text_new[i] = ids[texts[h][i] - 1];
            }
            if (text_new[i] == 0) {
                count_pad++;
            }
        }
        //texts[h] = text_new;
    }

    // sort types
    Types types_new(G);
    for (std::size_t g = 0; g < G; g++) {
        if (ids[g] != 0 && ids[g] != id_unused)
            types_new[ids[g] - 1] = types[g];
    }
    types = types_new;
    padded = count_pad > 0;
    
    // dev::Timer timer;
    // dev::start_timer("is_duplicated", timer);
    // is_duplicated(types);
    // dev::stop_timer("is_duplicated", timer);
    
    // Merge duplicated types -------------------
    
    //dev::start_timer("deduplication", timer);
    
    // Reset 
    id = 1; 
    std::fill(ids.begin(), ids.end(), 0);
    
    // Check duplicated or empty types
    bool duplicated;
    std::vector<bool> unique(G);
    std::unordered_map<std::string, unsigned int> map;
    for (std::size_t g = 0; g < G; g++) {
        if (types[g] == "") {
            ids[g] = 0;
        } else {
            auto it = map.insert(std::pair<std::string, unsigned int>(types[g], id));
            ids[g] = it.first->second;
            if (it.second) {
                unique[g] = true;
                id++; // increment iff there is no gap
            } else {
                duplicated = true;
            }
        }
    }
    //dev::stop_timer("deduplication", timer);
    
    if (duplicated) {
        for (std::size_t h = 0; h < H; h++) {
            Text &text_new = texts[h];
            std::size_t I = texts[h].size();
            for (std::size_t i = 0; i < I; i++) {
                if (texts[h][i] != 0)
                    text_new[i] = ids[texts[h][i] - 1];
            }
            //texts[h] = text_new;
        }
        
        Types types_new;
        types_new.reserve(G);
        for (std::size_t g = 0; g < G; g++) {
            if (unique[g])
                types_new.push_back(types[g]);
        }
        types = types_new;
    }

    recompiled = true;
    return;
}
