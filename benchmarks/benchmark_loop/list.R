Rcpp::sourceCpp('benchmarks/benchmark_loop/list.cpp')

loop_r <- function(list, values){
  lapply(list, function(x, y) x <- y, values)
}

numbers <- 1:26
toks <- rep(list(letters), 100000)
toks_hash <- rep(list(numbers), 100000)
microbenchmark::microbenchmark(
  loop_chr(toks, letters),
  loop_int(toks_hash, numbers),
  loop_defined_chr(toks, letters),
  loop_defined_int(toks_hash, numbers),
  loop_r(toks, letters),
  loop_r(toks, numbers))

