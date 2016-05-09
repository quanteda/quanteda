#include <Rcpp.h>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <vector>
//using namespace Rcpp;
using namespace std;

string join(const vector<string> tokens, 
                 const string delim){
  
  if(tokens.size() == 0) return "";
  string token_joined = tokens[0];
  for(int i = 1; i < tokens.size(); ++i){
    //Rcout << "Join " << i << ' ' << token_joined << "\n";
    token_joined = token_joined + delim + tokens[i];
  }
  return token_joined;
}

// [[Rcpp::export]]
int match_bit(const vector<string> &tokens1, 
              const vector<string> &tokens2){
  
  int len1 = tokens1.size();
  int len2 = tokens2.size();
  int len = min(len1, len2);
  int bit = 0;
  for (int i = 0; i < len; i++){
    bit += tokens1[i] == tokens2[i];
  }
  bit += len1 >= len2; // add one point for trailing space 
  return bit;
}

double sigma(vector<int> &counts, const int &n, const double &smooth){
  
  double s = 0;
  for (int b = 1; b <= n; b++){
    s += 1 / (counts[b] + smooth);
  }
  s += pow((n - 1), 2) / (counts[0] + smooth);
  return s;
}

double lambda(vector<int> &counts, const int &n, const double &smooth){
  
  double l = log(counts[n]+ smooth);
  for (int b = 1; b < n; b++){
    l -= log(counts[b] + smooth);
  }
  l += (n - 1) * log(counts[0] + smooth);
  return l;
}


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
Rcpp::List find_sequence_cppl(SEXP x,
                               const vector<string> &types,
                               const int &min){
  
  Rcpp::List texts(x);
  vector<string> tokens_seq;
  map<vector<string>, int> counts_seq;
  unordered_set<string> set_types (types.begin(), types.end());
  
  // Find all sequences of specified types
  int len = texts.size();
  for (int h = 0; h < len; h++){
    
    //Rcout << "Text " << h << "\n";
    vector<string> tokens = texts[h];
    vector<string> tokens_seq;
    
    int len = tokens.size();
    for (int i = 1; i < len; i++){ // ignore first words in sentences
      Rcpp::String token = tokens[i];
      if(token == "") continue;
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in){
        tokens_seq.push_back(token);
      }else{
        counts_seq[tokens_seq]++;
        //if(seqs[tokens_seq] >= min) Rcout << join(tokens_seq, "-") << ' ' << seqs[tokens_seq] << "\n";
        tokens_seq.clear();
      }
    }
  }
  
  // Find significance of sequences
  
  //vector<double> ms;
  vector<double> sigmas;
  vector<double> lambdas;
  Rcpp::List sequences;
  for(auto it1 = counts_seq.begin(); it1 != counts_seq.end(); ++it1 ){
    if(it1->first.size()  < 2) continue;
    if(it1->second < min) continue;
    // Initialize
    int n = it1->first.size();
    vector<int> counts_bit(n + 1);
    for(auto it2 = counts_seq.begin(); it2 != counts_seq.end(); ++it2 ){
      if(it2->second <  min) continue;
      if(it1->first == it2->first) continue; // do not compare with itself
      int bit = match_bit(it1->first, it2->first);
      counts_bit[bit] += it1->second;
      //Rcout << join(it1->first, "-") << " vs " << join(it2->first, "-") << ": "
      //      << bit << " x " << it1->second << "\n";
    }
    //double s = sigma(counts_bit, n, 0.5);
    //double l = lambda(counts_bit, n, 0.5);
    //double m = l - (3.29 * s);
    //Rcout << join(it1->first, "-") << ": " << m << "\n";
    //tokens_seq_temp.push_back(join(it1->first, "-"));
    sequences.push_back(it1->first);
    //ms.push_back(m);
    sigmas.push_back(sigma(counts_bit, n, 0.5));
    lambdas.push_back(lambda(counts_bit, n, 0.5));
  }
  return Rcpp::List::create(Rcpp::Named("sequence") = sequences,
                            //Rcpp::Named("mue") = ms,
                            Rcpp::Named("lambda") = lambdas,
                            Rcpp::Named("sigma") = sigmas
                            );
}