#include <Rcpp.h>
#include <set>
#include <map>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>

using namespace Rcpp;

// Function for development
void print_vector(const std::string label,
                  const std::vector<std::string> tokens){
  
  if(tokens.size() == 0) return;
  std::string token_joined = tokens[0];
  for(int i = 1; i < tokens.size(); ++i){
    token_joined = token_joined + " " + tokens[i];
  }
  Rcout << label << ": " << token_joined << "\n";
}

// [[Rcpp::export]]
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

double sigma(std::vector<int> &counts, 
             const int &n, 
             const double &smooth){
  
  double s = 0;
  for (int b = 1; b <= n; b++){
    s += 1 / (counts[b] + smooth);
  }
  double base = n - 1; 
  s += pow(base, 2) / (counts[0] + smooth);
  return std::sqrt(s);
}

double lambda(std::vector<int> &counts, 
              const int &n, 
              const double &smooth){
  
  double l = std::log(counts[n]+ smooth);
  for (int b = 1; b < n; b++){
    l -= std::log(counts[b] + smooth);
  }
  l += (n - 1) * log(counts[0] + smooth);
  return l;
}

// [[Rcpp::export]]
Rcpp::List find_sequence_cppl(List texts,
                              const std::vector<std::string> &types,
                              const int &count_min,
                              const double &smooth,
                              const bool &nested){
  
  //Rcpp::List texts(x);
  std::map<std::vector<std::string>, int> counts_seq; // unorderd_map cannot take vector as key
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  
  // Find all sequences of specified types
  for (int h = 0; h < texts.size(); h++){
    
    //Rcout << "Text " << h << "\n";
    std::vector<std::string> text = texts[h];
    std::vector<std::string> tokens_seq;
    text.push_back(""); // add empty token to include last words
    
    int len_text = text.size();
    //print_vector("Text", text);
    for (int i = 1; i < len_text; i++){ // scan texts ignoring first words in texts
      for (int j = i; j < len_text; j++){ // collect nested sequence starting from i
        //Rcout << i << " " << j << "\n";
        Rcpp::String token = text[j];
        bool is_in;
        if(token == ""){
          is_in = false;
        }else{
          is_in = set_types.find(token) != set_types.end();
        }
        if(is_in){
          //Rcout << "Match: " << token.get_cstring() << "\n";
          tokens_seq.push_back(token);
        }else{
          //Rcout << "Not match: " <<  token.get_cstring() << "\n";
          if(tokens_seq.size() > 1){
            counts_seq[tokens_seq]++;
            //print_vector("Sequence", tokens_seq);
          }
          //print_vector("Reset", tokens_seq);
          tokens_seq.clear();
          if(!nested) i = j; // jump if nested is false
          break;
        }
      }
    }
  }
  
  // Find significance of sequences
  
  //vector<double> ms;
  std::vector<double> sigmas;
  std::vector<double> lambdas;
  Rcpp::List sequences;
  for(auto it1 = counts_seq.begin(); it1 != counts_seq.end(); ++it1 ){
    if(it1->first.size() < 2) continue; // ignore single words
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
    //print_vector("Token", it1->first);
    //Rcout << m << "\n";
    //Rcout << join(it1->first, "-") << ": " << m << "\n";
    //tokens_seq_temp.push_back(join(it1->first, "-"));
    sequences.push_back(it1->first);
    sigmas.push_back(sigma(counts_bit, n, smooth));
    lambdas.push_back(lambda(counts_bit, n, smooth));
  }
  return Rcpp::List::create(Rcpp::Named("sequence") = sequences,
                            Rcpp::Named("lambda") = lambdas,
                            Rcpp::Named("sigma") = sigmas
                            );
}