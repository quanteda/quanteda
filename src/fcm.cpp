#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
#include <unordered_map>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp ;

// [[Rcpp::export]]
arma::sp_mat fcm_cpp(Rcpp::List &texts,
                    const CharacterVector &types,
                    const String &count,
                    const int &window,
                    const NumericVector &weights,
                    const bool &ordered,
                    const int &nvec) {
    int n_types = types.size();
    arma::umat index_mat(2,nvec);
    arma::vec w_values(nvec);
    std::unordered_map<String, int> id;
    for (int g = 0; g < types.size(); g++) {
        id[types[g]] = g ;                      
    }
    
    if (count == "boolean"){
        // Currently, support for arma::sp_mat is preliminary, 
        // for exmaple Mat.find(Mat > 1) is not supported, so "booleanize" in matrix level is not applicable
        arma::sp_mat bFcm(n_types, n_types);
        for (int h = 0; h < texts.size(); h++) {
            StringVector text = texts[h];
            int len = text.size();
            arma::sp_mat aFcm(n_types,n_types);
            for (int i = 0; i < text.size(); i++) {
                int id_i = id[text[i]];
                int j_int = i+1;
                int j_lim = std::min(i + window + 1, len);
                for(int j = j_int; j < j_lim; j++) {
                    int id_j = id[text[j]];
                    if (ordered){
                        aFcm(id_i,id_j) = 1;
                    }else{
                        if (id_i<id_j){
                            aFcm(id_i,id_j) = 1;
                        }else{
                            aFcm(id_j,id_i) = 1;
                        }
                    }
                }
            }
            bFcm += aFcm;
        }
        return bFcm;
    }else{
        // define weights 
        NumericVector window_weights;
        if (count == "frequency" || count == "boolean"){
            window_weights = NumericVector(window, 1.0);
        }else if(count == "weighted"){ 
            if (weights.size() == 1){
                window_weights = NumericVector(window);
                for (int i=1; i<=window; i++){
                    window_weights(i-1) = 1.0/i;
                }
            }else{
                window_weights = NumericVector(weights);
            }
        }
        
        int vFrom = 0;
        int vTo = 0; 
        for (int h = 0; h < texts.size(); h++) {
            StringVector text = texts[h];
            int len = text.size();
            
            // numeric vector to represent the text
            arma::urowvec text_vec(len);
            for (int i = 0; i < len; i++) {
                text_vec(i) = id[text[i]];
            }
            
            //pair up the numeric vector to locate the pair co_occurred.
            //for instance: text_vec[0:end-1] - text_vec[1:end] denotes all the pairs with the offset = 1;  
            for (int i = 0; i < window; i++){
                int length = len - i -1;
                vTo = vFrom + length -1;
                index_mat.row(0).subvec(vFrom, vTo) = text_vec.head(length);
                index_mat.row(1).subvec(vFrom, vTo) = text_vec.tail(length);
                w_values.subvec(vFrom, vTo).fill(window_weights[i]);
                vFrom = vTo + 1; 
            }
        }
        arma::sp_mat a_fcm(TRUE, index_mat.cols(0, vTo), w_values.head(vFrom), n_types, n_types);
        if (!ordered){
            a_fcm += trans(a_fcm); 
            a_fcm.diag() /= 2;
        }
        return a_fcm;
    }
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

#fcm_cpp(rep(list(letters), 100), letters,"window", 5,1,TRUE)
*/
