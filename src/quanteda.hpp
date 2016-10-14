using namespace Rcpp;
using namespace std;

#ifndef __QUANTEDA__
#define __QUANTEDA__

namespace quanteda{

    inline String join_character_vector(const CharacterVector &tokens, const String &delim){
        if(tokens.size() == 0) return "";
        String token = tokens[0];
        int len_ngram = tokens.size();
        for (int i = 1; i < len_ngram; i++) {
          token += delim;
          token += tokens[i];
          //Rcout << "Joined " << token.get_cstring()  << "\n";
        }
        token.set_encoding(CE_UTF8);
        return token;
      }
    
    inline std::string join_vector(std::vector< std::string > ngram, std::string delim){
        if(ngram.size() == 0) return "";
        std::string token_ngram = ngram[0];
        int len_ngram = ngram.size();
        for (int i = 1; i < len_ngram; i++) {
          token_ngram = token_ngram + delim + ngram[i];
        }
        return token_ngram;
    }

    // Tokens object
    class Tokens {
      public:
      Tokens(List documents_, List vocaburary_): documents(documents_), vocaburary(vocaburary_){}
      Rcpp::List documents, vocaburary;;
    };
}

#endif