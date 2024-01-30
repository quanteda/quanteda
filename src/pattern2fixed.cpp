#include "lib.h"
//#include "dev.h"
#include "utf8.h"
using namespace quanteda;

typedef std::unordered_multimap<std::string, unsigned int> MapIndex;
typedef std::string Pattern;
typedef std::vector<Pattern> Patterns;
typedef std::tuple<int, std::string, int> Config;
typedef std::vector<Config> Configs;

void index_types(const Config conf, const Types &types, MapIndex &index) {
    
    int len, side;
    std::string wildcard;
    std::tie(side, wildcard, len) = conf;
    //Rcout << "Side: " << side << " wildcard: " << wildcard << " len: " << len << "\n";
    
    for (std::size_t h = 0; h < types.size(); h++) {
        std::string value;
        if (side == 1) {
            if (len > 0) {
                value = utf8_sub_right(types[h], len);
            } else {
                value = utf8_sub_right(types[h], utf8_length(types[h]) + len);
            }
            if (!value.empty()) {
                index.emplace(wildcard + value, h);
                //index.insert(std::make_pair(wildcard + value, h));
                //Rcout << "Insert: " << wildcard + value << " " << h << "\n";
            }
        } else if (side == 2) {
            if (len > 0) {
                value = utf8_sub_left(types[h], len);
            } else {
                value = utf8_sub_left(types[h], utf8_length(types[h]) + len);
            }
            if (!value.empty()) {
                //index.insert(std::make_pair(value + wildcard, h));
                index.emplace(value + wildcard, h);
                //Rcout << "Insert: " << value + wildcard << " " << h << "\n";
            }
        } else {
            //index.insert(std::make_pair(types[h], h));
            index.emplace(types[h], h);
            //Rcout << "Insert: " << types[h] << " " << h << "\n";
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
    confs.shrink_to_fit();
    return confs;
}

// [[Rcpp::export]]
List cpp_index_types(const CharacterVector &patterns_, 
                     const CharacterVector &types_, 
                     const bool glob = true) {
    
    //dev::Timer timer;
    Patterns patterns = Rcpp::as<Patterns>(patterns_);
    Types types = Rcpp::as<Types>(types_);

    Configs confs = parse_patterns(patterns, glob);
    MapIndex index;

    //dev::start_timer("Index", timer);
    std::size_t G = confs.size();
    for (std::size_t g = 0; g < G; g++) {
        index_types(confs[g], types, index);
    }
    //dev::stop_timer("Index", timer);

    //dev::start_timer("List", timer);
    List result_(patterns.size());
    for (std::size_t i = 0; i < patterns.size(); i++) {
        std::string pat = patterns[i];
        IntegerVector value_(index.count(pat));
        std::size_t j = 0;
        auto range = index.equal_range(pat);
        for (auto it = range.first; it != range.second; ++it) {
            value_[j++] = it->second + 1;
        }
        result_[i] = sort_unique(value_);
    }
    result_.attr("names") = encode(patterns);
    //dev::stop_timer("List", timer);
    
    return result_;
}

/*** R
cpp_index_types(c("a*", "*b", "*c*"),
                c("bbb", "aaa", "ccc", "aa", "bb"))
cpp_index_types(c("跩", "跩*"), c("跩购鹇", "跩"))
*/
