require(quanteda)
quanteda_options(threads = 8)

#corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian_2016-2017.RDS")
toks_guard <- tokens(data_corpus_irishbudget2010)
#toks <- tokens(corp)
toks <- toks_guard
type <- types(toks)
stem <- char_wordstem(type, "porter")

type <- type[stringi::stri_detect_fixed(type, "-")]
type_split <- stringi::stri_split_fixed(type, "-")
toks_out <- tokens_replace(toks, type, type_split, case_insensitive = FALSE)
kw <- kwic(toks_out, head(type_split))

head(kw)

microbenchmark::microbenchmark(
    tokens_replace(toks, phrase(c("this is", "that is", "it is")), 
                   phrase(c("this was", "that was", "it was")), 
                   case_insensitive = FALSE),
    times = 10
)

microbenchmark::microbenchmark(
    tokens_replace(toks, type, stem, "fixed", case_insensitive = TRUE),
    tokens_replace(toks, type, stem, "fixed", case_insensitive = FALSE),
    tokens_replace(toks, type, stem, "glob", case_insensitive = FALSE),
    times = 1
)

profvis::profvis(
    tokens_replace(toks, type, stem, "glob", case_insensitive = FALSE)
)

profvis::profvis(
    tokens_replace(toks, type, stem, "fixed", case_insensitive = FALSE)
)

