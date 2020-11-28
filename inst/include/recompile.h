#include "lib.h"
#include "dev.h"
using namespace quanteda;

#if QUANTEDA_USE_TBB
typedef tbb::concurrent_vector<unsigned int> VecIds;
#else
typedef std::vector<unsigned int> VecIds;
#endif

inline bool has_padding(Texts &texts) {
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            if (texts[h][i] == 0) {
                return true;
            }
        }
    }
    return false;
}

inline bool is_duplicated(Types types){
    std::sort(types.begin(), types.end());
    if (types.size() <= 1) return false;
    for (std::size_t i = 0; i < types.size() - 1; i++) {
        // Treat null tokens as duplicate because 0 is reserved for them
        if (types[i] == "" || types[i] == types[i + 1]) { 
            return true;
        }
    }
    return false;
}

inline bool is_encoded(String delim_){
    if (delim_.get_encoding() > 0) {
        return true;
    }
    return false;
}

inline bool is_encoded(CharacterVector types_){
    for (unsigned int i = 0; i < (unsigned int)types_.size(); i++) {
        String type_ = types_[i];
        if (type_.get_encoding() > 0) {
            return true;
        }
    }
    return false;
}

inline CharacterVector encode(CharacterVector types_){
    for (unsigned int  i = 0; i < (unsigned int)types_.size(); i++) {
        String type_ = types_[i];
        type_.set_encoding(CE_UTF8);
        types_[i] = type_;
    }
    return(types_);
}

struct RecompileWorker : public Worker{
    
    Texts &texts;
    VecIds &ids_new;
    
    RecompileWorker(Texts &texts_, VecIds &ids_new_):
        texts(texts_), ids_new(ids_new_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                // if (texts[h][i] < 0 || ids_new.size() <= texts[h][i]) {
                //     throw std::range_error("Invalid new token ID");
                // }
                texts[h][i] = ids_new[texts[h][i]];
            }
        }
    }
};

inline Tokens recompile(Texts texts, 
                        Types types, 
                        const bool check_gap = true, 
                        const bool check_dup = true,
                        const bool check_encode = true){
    
    VecIds ids_new(types.size() + 1);
    ids_new[0] = 0; // reserved for padding
    unsigned int id_new = 1;
    std::vector<bool> flags_used(ids_new.size(), false);
    std::vector<bool> flags_unique(ids_new.size(), false);
    flags_unique[0] = true; // padding is always unique
    
    /// dev::Timer timer;
    
    // Check if IDs are all used
    bool all_used = false;
    if (check_gap) {
        // dev::start_timer("Check gaps", timer);
        for (std::size_t h = 0; h < texts.size(); h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                try {
                    flags_used[texts[h][i]] = true;
                } catch (const std::out_of_range& e) {
                    throw std::range_error("Invalid tokens object");
                }
            }
        }
        // Check all but padding
        all_used = std::all_of(flags_used.begin() + 1, flags_used.end(), [](bool v) { return v; });
        // dev::stop_timer("Check gaps", timer);
    } else {
        
        // Only check for padding
        flags_used[0] = has_padding(texts);
        // Fill all but padding
        std::fill(flags_used.begin() + 1, flags_used.end(), true); 
        all_used = true;
    }
    
    // for (std::size_t k = 1; k < flags_used.size(); k++) {
    //     Rcout << "USE '" << types[k - 1] << "' " << k << ": " << flags_used[k]<< "\n";
    // }
    
    // Check if types are duplicated
    bool all_unique = false;
    if (check_dup && is_duplicated(types)) {
        // dev::start_timer("Check duplication", timer);
        std::unordered_map<std::string, unsigned int> types_unique;
        for (std::size_t g = 1; g < ids_new.size(); g++) {
            if (!flags_used[g]) continue; // ignore unused
            if (types[g - 1] == "") {
                flags_used[0] = true;
                ids_new[g] = 0;
            } else {
                auto it = types_unique.insert(std::pair<std::string, unsigned int>(types[g - 1], id_new));
                ids_new[g] = it.first->second;
                if (it.second) {
                    flags_unique[g] = true;
                    id_new++; // increment iff there is no gap
                }
            }
            //Rcout << types.at(g - 1)<< ": " << g << " -> " << ids_new.at(g) << "\n";
        }
        all_unique = std::all_of(flags_unique.begin(), flags_unique.end(), [](bool v) { return v; });
        // dev::stop_timer("Check duplication", timer);
    } else {
        unsigned int id_new = 1;
        for (std::size_t g = 1; g < ids_new.size(); g++) {
            if (flags_used[g]) {
                ids_new[g] = id_new++;
            }
            //Rcout << types.at(g - 1)<< ": " << g << " -> " << ids_new.at(g)<< "\n";
        }
        std::fill(flags_unique.begin(), flags_unique.end(), true);
        all_unique = true;
    }
    
    // for (std::size_t k = 1; k < flags_unique.size(); k++) {
    //    Rcout << "UNIQUE '" << types[k - 1] << "' " << k << ": " << flags_unique[k]<< "\n";
    //}
    
    // Do nothing if all used and unique
    //Rcout << all_used << " " << all_unique << "\n";
    if (all_used && all_unique) {
        CharacterVector types_ = Rcpp::wrap(types);;
        if (check_encode) {
            types_ = encode(types_);
        }
        Tokens texts_ = Rcpp::wrap(texts);
        texts_.attr("class") = "tokens";
        texts_.attr("types") = types_;
        texts_.attr("padding") = (bool)flags_used[0];
        return texts_;
    }
    
    //dev::start_timer("Convert IDs", timer);
    
    // Convert old IDs to new IDs
#if QUANTEDA_USE_TBB
    RecompileWorker recompile_worker(texts, ids_new);
    parallelFor(0, texts.size(), recompile_worker);
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            // if (texts[h][i] < 0 || ids_new.size() <= texts[h][i]) {
            //     throw std::range_error("Invalid new token ID");
            // }
            // Rcout << "CONVERT "
            //       << texts[h][i] << " -> "
            //       << ids_new[texts[h][i]] << "\n";
            texts[h][i] = ids_new[texts[h][i]];
        }
    }
#endif
    
    std::vector<std::string> types_new;
    types_new.reserve(ids_new.size());
    for (std::size_t j = 1; j < ids_new.size(); j++) {
        if (flags_used[j] && flags_unique[j]) {
            types_new.push_back(types.at(j - 1));
        }
    }
    //dev::stop_timer("Convert IDs", timer);
    
    //dev::start_timer("Wrap", timer);
    Tokens texts_ = Rcpp::wrap(texts);
    //dev::stop_timer("Wrap", timer);
    CharacterVector types_new_ = Rcpp::wrap(types_new);
    if (check_encode) {
        // dev::start_timer("Encode", timer);
        types_new_ = encode(types_new_);
        // dev::stop_timer("Encode", timer);
    }
    texts_.attr("class") = "tokens";
    texts_.attr("types") = types_new_;
    texts_.attr("padding") = (bool)flags_used[0];
    return texts_;
    
}
