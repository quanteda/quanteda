#include "lib.h"
#include "dev.h"
using namespace quanteda;

typedef std::vector<std::string> StringText;
typedef std::vector<StringText> StringTexts;

// [[Rcpp::export]]
List cpp_serialize(List texts_, CharacterVector types_){
    
    dev::Timer timer;
    
    Types types = Rcpp::as<Types>(types_);
    StringTexts texts = Rcpp::as<StringTexts>(texts_);
    
    dev::start_timer("Register", timer);
    //std::unordered_map<std::string, unsigned int> map;
    tbb::concurrent_unordered_map<std::string, UintParam> map;
    for (size_t g = 0; g < types.size(); g++) {
        std::string type = types[g];
        auto it = map.insert(std::pair<std::string, UintParam>(types[g], g + 1));
    }
    dev::stop_timer("Register", timer);
    
    dev::start_timer("Serialize", timer);
    UintParam id = types.size();
    std::size_t H = texts.size();
    Texts temp(H);
    for (size_t h = 0; h < H; h++) {
        std::size_t I = texts[h].size();
        temp[h] = Text(I);
        for (size_t i = 0; i < I; i++) {
            auto it1 = map.find(texts[h][i]);
            if (it1 != map.end()) {
                temp[h][i] = it1->second;
            } else {
#if QUANTEDA_USE_TBB    
                //auto it2 = map.insert(std::pair<std::string, UintParam>(texts[h][i], id.fetch_and_increment()));
#else
                //auto it2 = map.insert(std::pair<std::string, UintParam>(texts[h][i], id++));
#endif
                //temp[h][i] = it2.first->second;
            }
        }
    }
    dev::stop_timer("Serialize", timer);
    List result_ = as_list(temp);
    return(result_);
}

/***R
lis <- replicate(10, sample(letters), simplify = FALSE)
types <- rep(letters, 1000)
out <- cpp_serialize(lis, types)


*/
