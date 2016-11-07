Rcpp::sourceCpp('benchmarks/benchmark_substitution/unorderedset.cpp')

set_r_chr <- function(tokens, types){
  tokens[(tokens %in% types)] <- ''
  return(tokens)
}

set_r_num <- function(tokens, types){
  tokens[(tokens %in% types)] <- 0
  return(tokens)
}

toks <- rep(letters, 1000)
toks_hash <- rep(1:26, 1000)
microbenchmark::microbenchmark(
  set_cpp_chr(toks, c('a', 'c', 'd')),
  set_cpp_num(toks_hash, c(1, 3, 4)),
  set_cpp_num(toks_hash, c(1L, 3L, 4L)),
  set_r_chr(toks, c('a', 'c', 'd')),
  set_r_num(toks_hash, c(1, 3, 4)),
  set_r_num(toks_hash, c(1L, 3L, 4L)),
  times = 1000
)

identical(set_r_num(toks_hash, c(1, 3, 4)), set_cpp_num(toks_hash, c(1, 3, 4)))
identical(set_r_chr(toks_hash, c(1, 3, 4)), set_cpp_chr(toks_hash, c(1, 3, 4)))

