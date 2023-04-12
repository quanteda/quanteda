#include "lib.h"
#include "dev.h"
#include "utf8.h"
using namespace quanteda;

#if QUANTEDA_USE_TBB
typedef tbb::concurrent_unordered_map<Type, tbb::concurrent_vector<int> > MapIndex;
#else 
typedef tbb::concurrent_unordered_map<Type, std::vector<int> > MapIndex;
#endif
typedef std::string Pattern;
typedef std::vector<Pattern> Patterns;
typedef std::tuple<int, std::string, int> Config;
typedef std::vector<Config> Configs;

void index_types(Types &types, MapIndex &index, Config conf) {
    
    int len, side;
    std::string wildcard;
    std::tie(side, wildcard, len) = conf;
    //Rcout << "Side: " << side << " wildcard: " << wildcard << " len: " << len << "\n";
    
    std::size_t H = types.size();
    Types temp(H);
    
    for (size_t h = 0; h < H; h++) {
        std::string value;
        if (side == 1) {
            if (len > 0) {
                value = utf8_sub_right(types[h], len);
            } else {
                value = utf8_sub_right(types[h], utf8_length(types[h]) + len);
            }
            if (value != "") {
                auto it = index.find(wildcard + value);
                if (it != index.end()) {
                    it->second.push_back(h);
                    //Rcout << "Insert: " << wildcard + value << " " << h << "\n";
                }
            } 
        } else if (side == 2) {
            if (len > 0) {
                value = utf8_sub_left(types[h], len);
            } else {
                value = utf8_sub_left(types[h], utf8_length(types[h]) + len);
            }
            if (value != "") {
                auto it = index.find(value + wildcard);
                if (it != index.end()) {
                    it->second.push_back(h);
                    //Rcout << "Insert: " << value + wildcard << " " << h << "\n";
                }
            }
        } else {
            auto it = index.find(types[h]);
            if (it != index.end()) {
                it->second.push_back(h);
                //Rcout << "Insert: " << types[h] << " " << h << "\n";
            }
        }
    }
}


Configs parse_patterns(Patterns patterns, bool glob = true) {
    
    Configs confs;
    confs.reserve(patterns.size());
    std::unordered_set<std::string> set_unique; 
    
    // for fixed
    confs.push_back({0, "", 0});
    if (!glob)
        return confs;
        
    // for glob
    for (size_t i = 0; i < patterns.size(); i++) {
        
        Config conf;
        std::string pattern = patterns[i];
        std::string left = utf8_sub_left(pattern, 1);
        std::string right = utf8_sub_right(pattern, 1);
        std::string key = ""; // for deduplication
        int len;
        
        if ((left == "*" || left == "!") && (right == "*" || right == "!"))
            continue;
        if (left == "*") {
            len = utf8_length(pattern) - 1;
            key = "L*" + std::to_string(len);
            conf = {1, "*", len};
        } else if (right == "*") {
            len = utf8_length(pattern) - 1;
            key = "R*" + std::to_string(len);
            conf = {2, "*", len};
        } else if (left == "?") {
            key = "L?";
            conf = {1, "?", -1};
        } else if (right == "?") {
            key = "R?";
            conf = {2, "?", -1};
        }
        
        if (key != "") {
            auto it = set_unique.find(key);
            if (it == set_unique.end()) {
                //Rcout << "Add: " << key << "\n";
                set_unique.insert(key);
                confs.push_back(conf);
            }
        }
    }
    
    return confs;
}

// [[Rcpp::export]]
List cpp_index_types(const CharacterVector &patterns_, 
                     const CharacterVector &types_, bool glob = true) {
    
    //dev::Timer timer;
    //dev::start_timer("Convert", timer);
    
    Patterns patterns = Rcpp::as<Patterns>(patterns_);
    Types types = Rcpp::as<Types>(types_);
    
    MapIndex index;
    for (size_t j = 0; j < patterns.size(); j++) {
        index[patterns[j]].reserve(types.size());
        //Rcout << "Register: " << patterns[j] << "\n";
    }
    Configs confs = parse_patterns(patterns, glob);

    //dev::stop_timer("Convert", timer);
    
    //dev::start_timer("Index", timer);
    
    std::size_t H = confs.size();
    Texts temp(H);
#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            index_types(types, index, confs[h]);
        }
    });
#else
    for (size_t h = 0; h < H; h++) {
        index_types(types, index, confs[h]);
    }
#endif
    //dev::stop_timer("Index", timer);

    //dev::start_timer("List", timer);
    List result_(patterns.size());
    for (size_t i = 0; i < patterns.size(); i++) {
        std::string pattern = patterns[i];
        IntegerVector value_ = Rcpp::wrap(index[pattern]);
        result_[i] = sort_unique(value_) + 1; // R is 1 base
    }
    result_.attr("names") = encode(patterns);
    //dev::stop_timer("List", timer);
    
    return result_;
}

/*** R
#out <- cpp_index_types(c("a*", "*b", "*c*", "跩*"), 
#                       c("bbb", "aaa", "跩购鹇", "ccc", "aa", "bb"))
cpp_index_types(c("跩", "跩*"), c("跩购鹇", "跩"))
*/
