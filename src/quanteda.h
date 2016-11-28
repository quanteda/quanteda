using namespace Rcpp;
using namespace std;

#ifndef __QUANTEDA__
#define __QUANTEDA__


#define RCPP_USING_CXX11
namespace quanteda{

    inline String join_character_vector(const CharacterVector &tokens, const String &delim){
        if(tokens.size() == 0) return "";
        String token = tokens[0];
        for (int i = 1; i < tokens.size(); i++) {
          token += delim;
          token += tokens[i];
          //Rcout << "Joined " << token.get_cstring()  << "\n";
        }
        token.set_encoding(CE_UTF8);
        return token;
      }
    
    inline std::string join_vector(std::vector< std::string > tokens, std::string delim){
        if(tokens.size() == 0) return "";
        std::string token = tokens[0];
        for (int i = 1; i < tokens.size(); i++) {
          token += delim + tokens[i];
        }
        return token;
    }

    inline bool has_na(IntegerVector vec) {
        for (int i = 0; i < vec.size(); ++i) {
            if(vec[i] == NA_INTEGER) return true;
        }
      return false;
    }
}

#endif
