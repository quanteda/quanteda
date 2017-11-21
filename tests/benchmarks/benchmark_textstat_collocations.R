load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian[1:1000]

toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
toks <- tokens_remove(toks, "\\p{S}", "regex", padding = TRUE)

microbenchmark::microbenchmark(
    textstat_collocations(toks, size = 2:5, min_count = 10),
    times = 10
)

head(out[out$count_nested / out$count < 0.1,], 20)
head(out, 20)
