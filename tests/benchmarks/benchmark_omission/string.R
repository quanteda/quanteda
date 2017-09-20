Rcpp::sourceCpp('src/utility.cpp')

txt <- rep(paste0(letters, collapse=' '), 10000)
toks <- tokens(txt, hash=FALSE)

identical(
  qatd_cpp_remove_chr_list(toks, 'd')[[1]],
  lapply(toks, function(x) x <- x[which(x != "d")])[[1]]
)

microbenchmark::microbenchmark(
  qatd_cpp_remove_chr_list(toks, 'd'),
  lapply(toks, function(x) x <- x[which(x != "d")]),
  unit='relative'
)
