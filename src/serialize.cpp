#include "lib.h"
//#include "dev.h"
using namespace quanteda;

typedef std::vector<std::string> StringText;
typedef std::vector<StringText> StringTexts;
#if QUANTEDA_USE_TBB
typedef tbb::concurrent_unordered_map<std::string, UintParam> MapTypes;
#else
typedef std::unordered_map<std::string, unsigned int> MapTypes;
#endif

Text serialize(const StringText &text, 
               MapTypes &map, 
               UintParam &id) {
    std::size_t I = text.size();
    Text temp(I);
    for (size_t i = 0; i < I; i++) {
        if (text[i] == "") {
            temp[i] = 0;
        } else {
            auto it1 = map.find(text[i]);
            if (it1 != map.end()) {
                temp[i] = it1->second;
            } else {
#if QUANTEDA_USE_TBB
                auto it2 = map.insert(std::pair<std::string, UintParam>(text[i], id.fetch_and_increment()));
#else
                auto it2 = map.insert(std::pair<std::string, UintParam>(text[i], id++));
#endif
                temp[i] = it2.first->second;
            }
        }
    }
    return temp;
}


// [[Rcpp::export]]
TokensPtr cpp_serialize(List texts_){
    
    //dev::Timer timer;
    StringTexts texts = Rcpp::as<StringTexts>(texts_);
    MapTypes map;
    
    //dev::start_timer("Serialize", timer);
    UintParam id = 1;
    std::size_t H = texts.size();
    Texts temp(H);
#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            temp[h] = serialize(texts[h], map, id);
        }    
    });
#else
    for (size_t h = 0; h < H; h++) {
        temp[h] = serialize(texts[h], map, id);
    }
#endif
    
    Types types(id - 1);
    for (std::pair<std::string, unsigned int> it : map) {
        types[it.second - 1] = it.first;
    }
    //dev::stop_timer("Serialize", timer);
    TokensObj *ptr = new TokensObj(temp, types);
    return TokensPtr(ptr, true);
}

// [[Rcpp::export]]
TokensPtr cpp_serialize_add(List texts_, TokensPtr xptr){
    
    //dev::Timer timer;
    Types types = xptr->types;
    StringTexts texts = Rcpp::as<StringTexts>(texts_);
    
    //dev::start_timer("Register", timer);
    MapTypes map;
    for (size_t g = 0; g < types.size(); g++) {
        std::string type = types[g];
        auto it = map.insert(std::pair<std::string, UintParam>(types[g], g + 1));
    }
    //dev::stop_timer("Register", timer);
    
    //dev::start_timer("Serialize", timer);
    UintParam id = types.size() + 1;
    std::size_t H = texts.size();
    Texts temp(H);
#if QUANTEDA_USE_TBB
    tbb::parallel_for(tbb::blocked_range<int>(0, H), [&](tbb::blocked_range<int> r) {
        for (int h = r.begin(); h < r.end(); ++h) {
            temp[h] = serialize(texts[h], map, id);
        }    
    });
#else
    for (size_t h = 0; h < H; h++) {
        temp[h] = serialize(texts[h], map, id);
    }
#endif
    
    Types types_new(id - 1);
    for (std::pair<std::string, unsigned int> it : map) {
        types_new[it.second - 1] = it.first;
    }
    //dev::stop_timer("Serialize", timer);

    xptr->texts.insert(xptr->texts.end(), temp.begin(), temp.end());
    xptr->types = types_new;
    return xptr;
}


/***R
lis1 <- replicate(10, sample(c("", letters)), simplify = FALSE)
lis2 <- replicate(10, sample(c("", LETTERS)), simplify = FALSE)

out1 <- cpp_serialize(lis1)
unclass(quanteda:::cpp_as_list(out1))
out2 <- cpp_serialize_add(lis2, out1)
unclass(quanteda:::cpp_as_list(out2))

*/
