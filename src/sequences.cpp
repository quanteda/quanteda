#include <Rcpp.h>
#include <set>
#include <map>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>


using namespace Rcpp;

int match_bit(const std::vector<std::string> &tokens1, 
              const std::vector<std::string> &tokens2){
  
  int len1 = tokens1.size();
  int len2 = tokens2.size();
  int len = std::min(len1, len2);
  int bit = 0;
  for (int i = 0; i < len; i++){
    bit += tokens1[i] == tokens2[i];
  }
  bit += len1 >= len2; // add one point for trailing space 
  return bit;
}

double sigma(std::vector<int> &counts, const int &n, const double &smooth){
  
  double s = 0;
  for (int b = 1; b <= n; b++){
    s += 1 / (counts[b] + smooth);
  }
  s += pow((n - 1), 2) / (counts[0] + smooth);
  return s;
}

double lambda(std::vector<int> &counts, const int &n, const double &smooth){
  
  double l = log(counts[n]+ smooth);
  for (int b = 1; b < n; b++){
    l -= log(counts[b] + smooth);
  }
  l += (n - 1) * log(counts[0] + smooth);
  return l;
}

// [[Rcpp::export]]
Rcpp::List find_sequence_cppl(List texts,
                              const std::vector<std::string> &types,
                              const int &count_min,
                              const int &len_max,
                              const double &smooth){
  
  //Rcpp::List texts(x);
  std::vector<std::string> tokens_seq;
  std::map<std::vector<std::string>, int> counts_seq; // unorderd_map cannot take vector as key
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  
  // Find all sequences of specified types
  int len = texts.size();
  for (int h = 0; h < len; h++){
    
    //Rcout << "Text " << h << "\n";
    std::vector<std::string> tokens = texts[h];
    std::vector<std::string> tokens_seq;
    
    int len = tokens.size();
    for (int i = 1; i < std::min(len, len_max); i++){ // has to ignore first words in sentences
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
  std::vector<double> sigmas;
  std::vector<double> lambdas;
  Rcpp::List sequences;
  for(auto it1 = counts_seq.begin(); it1 != counts_seq.end(); ++it1 ){
    if(it1->first.size()  < 2) continue;
    if(it1->second < count_min) continue;
    // Initialize
    int n = it1->first.size();
    std::vector<int> counts_bit(n + 1);
    for(auto it2 = counts_seq.begin(); it2 != counts_seq.end(); ++it2 ){
      if(it2->second <  count_min) continue;
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
    sigmas.push_back(sigma(counts_bit, n, smooth));
    lambdas.push_back(lambda(counts_bit, n, smooth));
  }
  return Rcpp::List::create(Rcpp::Named("sequence") = sequences,
                            //Rcpp::Named("mue") = ms,
                            Rcpp::Named("lambda") = lambdas,
                            Rcpp::Named("sigma") = sigmas
                            );
}