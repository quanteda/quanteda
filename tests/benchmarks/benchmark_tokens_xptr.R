#devtools::install_github("quanteda/quanteda3")
require(quanteda)
corp <- readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds') %>% 
    corpus_reshape()

toks <- tokens(corp, remove_punct = FALSE, remove_numbers = FALSE, 
               remove_symbols = FALSE)
xtoks <- as.tokens_xptr(toks)
class(toks)
class(xtoks)

microbenchmark::microbenchmark(
    old = quanteda3::tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, 
                            remove_symbols = TRUE),
    new = tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, 
                 remove_symbols = TRUE),
    times = 1
)

microbenchmark::microbenchmark(
    old = quanteda3::tokens(toks, remove_punct = TRUE, remove_numbers = TRUE, 
                           remove_symbols = TRUE),
    new = tokens(xtoks, remove_punct = TRUE, remove_numbers = TRUE, 
                 remove_symbols = TRUE),
    times = 10
)

toks2 <- quanteda3::tokens_select(toks, data_dictionary_LSD2015)
xtoks2 <- tokens_select(as.tokens_xptr(xtoks), data_dictionary_LSD2015)
identical(as.list(toks2), as.list(xtoks2))

microbenchmark::microbenchmark(
    old = quanteda3::tokens_remove(toks, stopwords("en"), padding = TRUE) %>% 
        quanteda3::tokens_remove("[\\p{N}]", valuetype = "regex", padding = TRUE) %>% 
        quanteda3::tokens_remove("[\\p{P}]", valuetype = "regex", padding = TRUE) %>% 
        quanteda3::tokens_remove("[\\p{S}]", valuetype = "regex", padding = TRUE) %>% 
        quanteda3::dfm(remove_padding = TRUE),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_remove(stopwords("en"), padding = TRUE) %>% 
        tokens_remove("[\\p{N}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{P}]", valuetype = "regex", padding = TRUE) %>% 
        tokens_remove("[\\p{S}]", valuetype = "regex", padding = TRUE) %>% 
        dfm(remove_padding = TRUE),
    times = 10
)

microbenchmark::microbenchmark(
    old = quanteda3::tokens_compound(toks, data_dictionary_LSD2015),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_compound(data_dictionary_LSD2015),
    times = 10
)

microbenchmark::microbenchmark(
    old = quanteda3::tokens_compound(toks, "&", window = 1),
    new = as.tokens_xptr(xtoks) %>% 
        tokens_compound("&", window = 1),
    times = 10
)

microbenchmark::microbenchmark(
    old = quanteda3::dfm(toks),
    news = dfm(xtoks),
    times = 10
)
identical(dfm(xtoks), quanteda3::dfm(toks))

microbenchmark::microbenchmark(
    old = quanteda3::tokens_group(toks),
    new = tokens_group(xtoks),
    times = 10
)
identical(as.tokens(tokens_group(xtoks)), quanteda3::tokens_group(toks))

profvis::profvis(
    dfm(xtoks)
)

profvis::profvis(
    tokens_select(as.tokens_xptr(xtoks), stopwords(), padding = TRUE)
)

profvis::profvis(
    tokens_select(toks, stopwords(), padding = TRUE)
)

