#include "lib.h"
//#include "dev.h"
using namespace quanteda;

#if QUANTEDA_USE_TBB
typedef tbb::concurrent_vector<unsigned int> VecIds;
#else
typedef std::vector<unsigned int> VecIds;
#endif

class TokensObj {
    public:
        TokensObj(Texts texts_, Types types_): texts(texts_), types(types_){}
        Texts texts;
        Types types;
        bool padding = true;
        
        void recompile(bool flag_gap, bool flag_dup);
        // length();
        // lengths();
    private:
        bool is_duplicated();
};

inline bool TokensObj::is_duplicated() {
    std::sort(types.begin(), types.end());
    if (types.size() <= 1) return false;
    for (std::size_t i = 0; i < types.size() - 1; i++) {
        if (types[i] != "" && types[i] == types[i + 1]) {
            return true;
        }
    }
    return false;
}

inline void TokensObj::recompile(bool flag_gap = true, bool flag_dup = true) {

    VecIds ids_new(types.size() + 1);
    ids_new[0] = 0; // reserved for padding
    unsigned int id_new = 1;
    std::vector<bool> flags_used(ids_new.size(), false);
    std::vector<bool> flags_unique(ids_new.size(), false);

    /// dev::Timer timer;
  
    // Check if IDs are all used
    bool all_used;
    if (flag_gap) {
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
    if (flag_dup && is_duplicated()) {
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
        return;
    }

    // Convert old IDs to new IDs
#if QUANTEDA_USE_TBB
    std::size_t H = texts.size();
    int g = std::ceil(H / tbb::this_task_arena::max_concurrency());
    tbb::parallel_for(tbb::blocked_range<int>(0, H, g), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                texts[h][i] = ids_new[texts[h][i]];
            }
        }
    }, tbb::auto_partitioner());
#else
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            texts[h][i] = ids_new[texts[h][i]];
            //Rcout << texts[h][i] << " -> " << ids_new[texts[h][i]] << "\n";
        }
    }
#endif

    Types types_new;
    types_new.reserve(ids_new.size());
    for (std::size_t j = 0; j < ids_new.size() - 1; j++) {
        if (flags_used[j + 1] && flags_unique[j + 1]) {
            types_new.push_back(types[j]);
        }
    }
    types = types_new;
    padding = (bool)flags_used[0];
}

// XPtr objects -------------------------------------------------------

typedef XPtr<TokensObj> TokensPtr;
