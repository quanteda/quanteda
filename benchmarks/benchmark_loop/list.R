Rcpp::sourceCpp('benchmarks/benchmark_loop/list.cpp')

numbers <- 1:26
toks <- rep(list(LETTERS), 100000)
toks_hash <- rep(list(numbers), 100000)
microbenchmark::microbenchmark(list_undefined(toks, letters),
                               list_defined_numeric(toks_hash, numbers),
                               list_defined_charactor(toks, letters))

toks_out1 <- list_undefined(toks, letters)
object.size(toks_out1)
toks_out2 <- list_defined_charactor(toks, letters)
object.size(toks_out2)