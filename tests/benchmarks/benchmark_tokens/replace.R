require(quanteda)
quanteda_options(threads = 8)

corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian_2016-2017.RDS")
toks <- tokens(corp)
#toks <- tokens(data_corpus_irishbudget2010)
type <- types(toks)
stem <- char_wordstem(type, "porter")
out <- tokens_replace2(toks, type, stem, case_insensitive = FALSE)

microbenchmark::microbenchmark(
    tokens_replace(toks, type, stem, case_insensitive = FALSE),
    tokens_replace(toks, phrase(type), phrase(stem), case_insensitive = FALSE),
    times = 1
)

profvis::profvis(
    tokens_replace2(toks, type, stem, case_insensitive = FALSE)
)
