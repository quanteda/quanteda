#include <RcppArmadillo.h>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

typedef std::vector<unsigned int> Text;
typedef std::vector<Text> Texts;
typedef std::string Type;
typedef std::vector<Type> Types;
typedef std::vector<unsigned int> Ids;

class TokensObj {
    public:
        TokensObj(Texts texts_, Types types_, bool recompiled_ = false): 
                  texts(texts_), types(types_), recompiled(recompiled_){}
        
        // variables
        Texts texts;
        Types types;
        bool recompiled;
        
        // functions
        void recompile();
        void recompile2();

    private:
        bool is_duplicated(Types types);
};

inline bool TokensObj::is_duplicated(Types types) {
    std::sort(types.begin(), types.end());
    if (types.size() <= 1) return false;
    for (std::size_t i = 0; i < types.size() - 1; i++) {
        if (types[i] != "" && types[i] == types[i + 1]) {
            return true;
        }
    }
    return false;
}

inline void TokensObj::recompile() {

    Ids ids_new(types.size() + 1);
    ids_new[0] = 0; // reserved for padding
    unsigned int id_new = 1;
    std::vector<bool> flags_used(ids_new.size(), false);
    std::vector<bool> flags_unique(ids_new.size(), false);

    /// dev::Timer timer;
  
    // Check if all IDs are used
    bool all_used;
    if (!recompiled) {
        // dev::start_timer("Check gaps", timer);
        unsigned int id_limit = ids_new.size();
        for (std::size_t h = 0; h < texts.size(); h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                unsigned int id = texts[h][i];
                if (id > id_limit) {
                    throw std::range_error("Invalid tokens object");
                }
                flags_used[id] = true;
            }
        }
        all_used = std::all_of(flags_used.begin(), flags_used.end(), [](bool v) { return v; });
        // dev::stop_timer("Check gaps", timer);
    } else {
        // Mark all types but padding are used
        std::fill(flags_used.begin() + 1, flags_used.end(), true);
        
        // Only check for padding
        for (std::size_t h = 0; h < texts.size() && !flags_used[0]; h++) {
            for (std::size_t i = 0; i < texts[h].size() && !flags_used[0]; i++) {
                if (texts[h][i] == 0) {
                    flags_used[0] = true;
                }
            }
        }
        all_used = true;
    }
    
    // Check if types are duplicated
    bool all_unique;
    if (!recompiled && is_duplicated(types)) {
        std::unordered_map<std::string, unsigned int> types_unique;
        flags_unique[0] = true; // padding is always unique
        for (std::size_t g = 1; g < ids_new.size(); g++) {
            if (types[g - 1] == "") continue; // ignore null types
            if (!flags_used[g]) continue; // ignore unused
            auto it = types_unique.insert(std::pair<std::string, unsigned int>(types[g - 1], id_new));
            ids_new[g] = it.first->second;
            if (it.second) {
                flags_unique[g] = true;
                id_new++; // increment iff there is no gap
            }
        }
        all_unique = std::all_of(flags_unique.begin(), flags_unique.end(), [](bool v) { return v; });
    } else {
        unsigned int id_new = 1;
        for (std::size_t g = 1; g < ids_new.size(); g++) {
            if (flags_used[g]) {
                ids_new[g] = id_new++;
            }
        }
        std::fill(flags_unique.begin(), flags_unique.end(), true);
        all_unique = true;
    }
    
    // Do nothing if all used and unique
    if (all_used && all_unique) {
        recompiled = true;
        return;
    }

    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            texts[h][i] = ids_new[texts[h][i]];
            //Rcout << texts[h][i] << " -> " << ids_new[texts[h][i]] << "\n";
        }
    }

    Types types_new;
    types_new.reserve(ids_new.size());
    for (std::size_t j = 0; j < ids_new.size() - 1; j++) {
        if (flags_used[j + 1] && flags_unique[j + 1]) {
            types_new.push_back(types[j]);
        }
    }
    types = types_new;
    recompiled = true;
    return;
}

inline void TokensObj::recompile2() {
    
    if (recompiled) {
        return;
    }
    
    Rcout << "Re-index tokens\n";
    
    // Re-index tokens -------------------
    
    std::size_t G = types.size();
    unsigned int id = 1;
    std::vector<unsigned int> ids(G, 0);
    unsigned int id_limit = G;
    int count_pad = 0;
    
    // NOTE: do we need thses?
    bool asis = false; 
    
    std::size_t H = texts.size();
    for (std::size_t h = 0; h < H; h++) {
        
        std::size_t I = texts[h].size();
        Text text_new(I);
        for (std::size_t i = 0; i < I; i++) {
            
            if (texts[h][i] > id_limit) 
                throw std::range_error("Invalid tokens object");
            if (texts[h][i] == 0) {
                text_new[i] = 0;
                count_pad++;
            } else {
                if (asis) {
                    text_new[i] = texts[h][i]; // for dictionary
                } else {
                    if (ids[texts[h][i] - 1] == 0) {
                        ids[texts[h][i] - 1] = id;
                        id++; // do not increment if duplicated?
                    }
                    text_new[i] = ids[texts[h][i] - 1];
                }
            }
        }
        texts[h] = text_new;
    }

    
    // sort types
    Types types_new(G);
    if (!asis) {
        for (std::size_t g = 0; g < G; g++) {
            if (ids[g] != 0) // zero if the types are not used
                types_new[ids[g] - 1] = types[g];
        }
    }
    types = types_new;
    
    // Merge duplicated types -------------------
    
    if (!is_duplicated(types)) {
        recompiled = true;
        return;
    } else {
        Rcout << "Merge duplicate types\n";
        
        // Check if types are duplicated
        unsigned int id = 1;
        std::vector<unsigned int> ids(G, 0);
        std::vector<double> unique(G);
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
            if (unique[g]) {
                types_new.push_back(types[g]);
            }
        }
        types = types_new;
        
        recompiled = true;
        return;
    }
}
