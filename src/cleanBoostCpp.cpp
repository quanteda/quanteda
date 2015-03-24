// NOTES:
// I think we could improve this by concatenating the regexes (except the substitution
// based regexes) rather than calling separate ones.
/*
#include <Rcpp.h>
#include <string.h>
#include <boost/regex.hpp>

using namespace Rcpp;
using namespace std;
using namespace boost;

boost::regex re_digits2 ("[[:digit:]]");
boost::regex re_punct2("[[:punct:]]");
boost::regex re_twitter2("(^|\\s)(#|@)\\S+");
boost::regex re_url2("(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\".,<>?]))");
boost::regex re_multipleSpaces("\\s{2,}");

// [[Rcpp::export]]
Rcpp::CharacterVector cleanBoostCpp(SEXP x, 
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
    // const std::string blank ("");
    
    // Regexp cleaning
    if (rm_digts) 
        boost::regex_replace (&str, re_digits2, "");
    if (rm_punct) 
        boost::regex_replace (&str, re_punct2, "");
    if (rm_twitter) 
        boost::regex_replace (&str, re_twitter2, " ");
    if (rm_url) 
        boost::regex_replace (&str, re_url2, "");
    if (rm_addit.length() > 0) {
        try {
            const std::regex re_addit2(rm_addit);
            boost::regex_replace (&str, re_addit2, "");
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
    boost::regex_replace (&str, re_multipleSpaces, " ");

    return wrap(str);
}
*/