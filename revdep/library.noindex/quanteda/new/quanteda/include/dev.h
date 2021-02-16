#include <chrono>
#include <string>
#include <map>

#ifndef __DEV__
#define __DEV__

using namespace Rcpp;

namespace dev{
    
    
    /* ---- Profiling tools ------------------------- */
    
    typedef std::map<std::string, std::chrono::time_point<std::chrono::high_resolution_clock> > Timer;
    
    inline void start_timer(std::string label, Timer &timer){
        auto now = std::chrono::high_resolution_clock::now();
        timer[label] = now;
    }
    
    inline void stop_timer(std::string label, Timer &timer){
        if (timer.find(label) == timer.end()){
            Rcout << std::left << std::setw(20) << "'" + label + "'";
            Rcout << " is not timed\n";
        }else{
            Rcout << std::left << std::setw(20) <<  "'" + label + "'";
            Rcout << " ";
            auto now = std::chrono::high_resolution_clock::now();
            Rcout << std::chrono::duration<double, std::milli>(now - timer[label]).count();
            Rcout << " millsec\n";
        }
    }
    
    /* ---- Debugging tools ------------------------- */
    
    inline void print_ngram(std::vector<unsigned int> vec){
        for(auto elm : vec){
            Rcout << std::to_string(elm) << " ";
        }
        Rcout << "\n";
    }
    
    inline void print_ngram(std::vector<bool> vec){
        for(auto elm : vec){
            Rcout << (int)elm << "";
        }
        Rcout << "\n";
    }
    
    inline void print_ngram(std::vector<std::string> vec){
        for(auto elm : vec){
            Rcout << elm << " ";
        }
        Rcout << "\n";
    }
}

#endif
