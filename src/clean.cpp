// NOTES:
// I think we could improve this by concatenating the regexes (except the substitution
// based regexes) rather than calling separate ones.

#include <Rcpp.h>
#include <string.h>
#include <pcrecpp.h>

using namespace Rcpp;
using namespace std;
using namespace pcrecpp;

pcrecpp::RE re_digits2("[[:digit:]]");
pcrecpp::RE re_punct2("[[:punct:]]");
pcrecpp::RE re_twitter2("(^|\\s)(#|@)\\S+");
pcrecpp::RE re_url2("(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\".,<>?]))");
pcrecpp::RE re_multipleSpaces("\\s{2,}");

// [[Rcpp::export]]
Rcpp::CharacterVector cleancpp(SEXP x, 
                       SEXP toLower, 
                       SEXP removeDigits,
                       SEXP removePunct,
                       SEXP removeTwitter,
                       SEXP removeURL,
                       SEXP removeAdditional) {
  
    std::string str = Rcpp::as <string> (x); 
    bool to_lower = Rcpp::as <bool> (toLower);
    bool rm_digts = Rcpp::as <bool> (removeDigits);
    bool rm_punct = Rcpp::as <bool> (removePunct);
    bool rm_twitter = Rcpp::as <bool> (removeTwitter);
    bool rm_url = Rcpp::as <bool> (removeURL);
  
    std::string rm_addit = Rcpp::as <string> (removeAdditional);
  
    // Regexp cleaning
    if (rm_digts) 
        re_digits2.GlobalReplace("", &str);
    if (rm_punct) 
        re_punct2.GlobalReplace("", &str);
    if (rm_twitter) 
        re_twitter2.GlobalReplace(" ", &str);
    if (rm_url) 
        re_url2.GlobalReplace("", &str);
    if (rm_addit.length() > 0) {
        try {
            pcrecpp::RE re_addit2(rm_addit);
            re_addit2.GlobalReplace("", &str);
        } catch(std::exception& e) {
            Rcout << "Invalid regular expression given: " <<  rm_addit << "\n";
        }
    }
    
    if (to_lower) {
        int i = 0;
        while (str[i]) {
            str[i] = tolower(str[i]);
            i++;
        }
    }

    // change 2+ spaces to a single space
    re_multipleSpaces.GlobalReplace(" ", &str);

    return wrap(str);
}
