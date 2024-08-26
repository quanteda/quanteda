//#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

typedef std::vector<unsigned int> Text;
typedef std::vector<Text> Texts;
typedef std::string Type;
typedef std::vector<Type> Types;
typedef std::vector<unsigned int> Ids;

class TokensObj {
    public:
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
        bool is_duplicated(Types types);
};

inline bool TokensObj::is_duplicated(Types types) {
    std::sort(types.begin(), types.end());
    if (types.size() <= 1) return false;
    for (std::size_t i = 0; i < types.size() - 1; i++) {
        if (types[i] == "" || types[i] == types[i + 1]) {
            return true;
        }
    }
    return false;
}

inline void TokensObj::recompile() {
    
    if (recompiled) return; // do nothing
    
    std::size_t G = types.size();
    std::size_t H = texts.size();
    unsigned int id_limit = G;
    unsigned int id_unused = id_limit + 1;
    
    //Rcout << "Re-index tokens\n";
    
    // Re-index tokens -------------------
    
    unsigned int id = 1;
    std::vector<unsigned int> ids(G, id_unused);
    for (std::size_t g = 0; g < G; g++) {
        if (types[g] == "") 
            ids[g] = 0; // padding
    }
    int count_pad = 0;
    for (std::size_t h = 0; h < H; h++) {
        std::size_t I = texts[h].size();
        Text text_new(I);
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
        texts[h] = text_new;
    }

    // sort types
    Types types_new(G);
    for (std::size_t g = 0; g < G; g++) {
        if (ids[g] != 0 && ids[g] != id_unused)
            types_new[ids[g] - 1] = types[g];
    }
    types = types_new;
    padded = count_pad > 0;
    
    // Merge duplicated types -------------------
    
    if (is_duplicated(types)) {
        //Rcout << "Merge duplicate types\n";
        
        // Check duplicated or empty types
        unsigned int id = 1;
        std::vector<unsigned int> ids(G, 0);
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
                }
            }
        }
    
        for (std::size_t h = 0; h < H; h++) {
            std::size_t I = texts[h].size();
            Text text_new(I);
            for (std::size_t i = 0; i < I; i++) {
                if (texts[h][i] != 0)
                    text_new[i] = ids[texts[h][i] - 1];
            }
            texts[h] = text_new;
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
