#include <chrono>
#include <string>
#include <map>

#ifndef __DEV__
#define __DEV__

using namespace Rcpp;

namespace dev{

    std::map<std::string, std::chrono::time_point<std::chrono::high_resolution_clock> > timer;

    inline void print_ngram(std::vector<int> vec){
        for(auto elm : vec){
          Rcout << std::to_string(elm) << " ";
        }
          Rcout << "\n";
    }
    
    inline void print_ngram(std::vector<std::string> vec){
        for(auto elm : vec){
          Rcout << elm << " ";
        }
        Rcout << "\n";
    }
    
    inline void start_timer(std::string label){
        auto now = std::chrono::high_resolution_clock::now();
        timer[label] = now;
    }
    
    inline void stop_timer(std::string label){
        if (timer.find(label) == timer.end()){
            Rcout << std::left << std::setw(20) << "'" + label + "'";
            Rcout << " is not started\n";
        }else{
            Rcout << std::left << std::setw(20) <<  "'" + label + "'";
            Rcout << " ";
            auto now = std::chrono::high_resolution_clock::now();
            Rcout << std::chrono::duration<double, std::milli>(now - timer[label]).count();
            Rcout << " millsec\n";
        }
    }

}

#endif