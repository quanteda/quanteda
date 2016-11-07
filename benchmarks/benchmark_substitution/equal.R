Rcpp::sourceCpp('benchmarks/benchmark_substitution/equal.cpp')

equal_r_chr <- function(tokens, type){
  tokens[tokens == type] <- ''
  return(tokens)
}

equal_r_num <- function(tokens, type){
  tokens[tokens == type] <- 0
  return(tokens)
}

toks <- rep(letters, 1000)
toks_hash <- rep(1:26, 1000)
microbenchmark::microbenchmark(
  equal_cpp_chr(toks, 'd'),
  equal_cpp_num(toks_hash, 4),
  equal_cpp_num(toks_hash, 4L),
  equal_r_chr(toks, 'd'),
  equal_r_num(toks_hash, 4),
  equal_r_num(toks_hash, 4L),
  times = 10000
)

identical(equal_r_chr(toks_hash, 'd'), equal_cpp_chr(toks_hash, 'd'))
identical(equal_r_num(toks_hash, 4), equal_cpp_num(toks_hash, 4))
