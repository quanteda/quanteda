load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian[1:10000]

toks <- tokens_remove(toks, stopwords("english"), padding = TRUE)
toks <- tokens_remove(toks, "\\p{S}", "regex", padding = TRUE)
out <- textstat_collocations(toks, size = 2:5, min_count = 10)
head(out[out$count_nested / out$count < 0.1,], 20)
head(out, 20)
