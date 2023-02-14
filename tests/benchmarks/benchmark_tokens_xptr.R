require(quanteda)
corp <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds') %>% 
    corpus_reshape()

toks <- tokens(corp, remove_punct = FALSE, remove_numbers = FALSE, 
               remove_symbols = FALSE)
xtoks <- as.tokens_xptr(toks)
class(toks)
class(xtoks)

toks2 <- tokens_select(toks, data_dictionary_LSD2015)
xtoks2 <- tokens_select(as.tokens_xptr(xtoks), data_dictionary_LSD2015)
identical(toks2, as.tokens(xtoks2))

microbenchmark::microbenchmark(
    old = tokens_remove(toks, stopwords("en"), padding = TRUE) %>% 
        tokens_remove("[\\p{N}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{P}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{S}]", valuetype = "regex", padding = TRUE) %>% 
        dfm(),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_remove(stopwords("en"), padding = TRUE) %>% 
        tokens_remove("[\\p{N}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{P}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{S}]", valuetype = "regex", padding = TRUE) %>% 
        dfm(),
    times = 10
)

microbenchmark::microbenchmark(
    old = tokens_compound(toks, data_dictionary_LSD2015),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_compound(data_dictionary_LSD2015),
    times = 10
)

microbenchmark::microbenchmark(
    old = tokens_compound(toks, "&", window = 1),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_compound("&", window = 1),
    times = 10
)

microbenchmark::microbenchmark(
    dfm(toks),
    dfm(xtoks),
    times = 10
)
identical(dfm(xtoks), dfm(toks))

microbenchmark::microbenchmark(
    dfm(tokens_group(xtoks)),
    dfm_group(dfm(xtoks)),
    times = 10
)
identical(dfm_group(dfmt), dfm(tokens_group(xtoks)))

profvis::profvis(
    dfm(xtoks)
)

profvis::profvis(
    tokens_select(as.tokens_xptr(xtoks), stopwords(), padding = TRUE)
)

profvis::profvis(
    tokens_select(toks, stopwords(), padding = TRUE)
)

