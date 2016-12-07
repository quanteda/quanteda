Rcpp::sourceCpp('src/tokens_join.cpp')
Rcpp::sourceCpp('src/tokens_join_hash.cpp')

toks <- rep(letters, 10000)
toks_hash <- rep(1:26, 10000)

microbenchmark::microbenchmark(
  qatd_cpp_replace_hash_vector(toks_hash, c(3L, 4L), 9999),
  #join_tokens_cpp(toks, c('c', 'd'), '_'),
  quanteda:::matchSequence(c(3L, 4L), toks_hash), times=100
)