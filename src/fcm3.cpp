#include <Rcpp.h>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_map>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]

List fcm_cpp3(List &texts,
             const CharacterVector &types,
             const String &count,
             const int &window,
             const NumericVector &weights,
             const bool &ordered,
             const int &n) {
    IntegerVector index_tgt(n);
    IntegerVector index_col(n);
    NumericVector xValue(n);
    std::unordered_map<String, int> id;
    for (int g = 0; g < types.size(); g++) {
        id[types[g]] = g + 1;
    }
    
    int k = 0;
    for (int h = 0; h < texts.size(); h++) {
        StringVector text = texts[h];
        int len = text.size();
        for (int i = 0; i < text.size(); i++) {
            int id_i = id[text[i]];
            int j_int = i+1;
            int j_lim = std::min(i + window + 1, len);
            for(int j = j_int; j < j_lim; j++) {
                if(i==j) continue;
                int id_j = id[text[j]];
                //Rcout << k << " " << id_i << " " << id_j << "\n";
                if (ordered){
                    index_tgt[k] = id_i;
                    index_col[k] = id_j;
                }else{
                    if (id_i < id_j){
                        index_tgt[k] = id_i;
                        index_col[k] = id_j;
                    }else{
                        index_tgt[k] = id_j;
                        index_col[k] = id_i;
                    }
                }
                if (count == "frequency" || count == "boolean"){
                    xValue[k] = 1;
                }else if(count == "weighted"){ 
                    if (weights.size() == 1){
                        xValue[k] = 1.0/(j - i);  // be default, weights=1/offset
                    }else{
                        xValue[k] = weights[j - j_int];
                    }
                }
                k++;
                
            }
        }
    }
    return List::create(Named("target") = index_tgt[seq(0, k - 1)], 
                      Named("collocate") = index_col[seq(0, k - 1)],
                      Named("values") = xValue[seq(0, k - 1)]);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

#fcm_cpp(rep(list(letters), 100), letters, 5, 26 * 100 * (5 * 2))
#fcm_cpp(rep(list(letters), 100), letters, 5, 26 * 100 * (5 * 2))
*/
