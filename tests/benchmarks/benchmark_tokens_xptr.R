#devtools::install_github("quanteda/quanteda3")
require(quanteda)
corp <- #readRDS('/home/kohei/Dropbox/Public/data_corpus_guardian2016-10k.rds') %>% 
    readRDS('C:/Users/watan/Dropbox/Public/data_corpus_guardian2016-10k.rds') %>% 
    corpus_reshape()

toks <- tokens(corp, remove_punct = FALSE, remove_numbers = FALSE, 
               remove_symbols = FALSE)
xtoks <- as.tokens_xptr(toks)

class(toks)
class(xtoks)

identical(types(xtoks),
          quanteda3::types(toks))

identical(dfm(xtoks),
          quanteda3::dfm(toks))

identical(as.list(tokens_select(as.tokens_xptr(xtoks), data_dictionary_LSD2015)),
          as.list(quanteda3::tokens_select(toks, data_dictionary_LSD2015)))

identical(as.list(as.tokens(tokens_group(as.tokens_xptr(xtoks)))), 
          as.list(quanteda3::tokens_group(toks)))

# NOTE: column order must be unique
identical(quanteda3::dfm(tokens_ngrams(toks)),
          dfm(tokens_ngrams(as.tokens_xptr(xtoks))))

identical(dfm(tokens_ngrams(as.tokens_xptr(xtoks))),
          dfm(tokens_ngrams(as.tokens_xptr(xtoks))))

microbenchmark::microbenchmark(
    old = quanteda3::dfm(toks),
    new = dfm(xtoks),
    times = 10
)

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
    new = as.tokens_xptr(toks) %>% 
        tokens(remove_punct = TRUE, remove_numbers = TRUE, 
               remove_symbols = TRUE),
    times = 10
)

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

microbenchmark::microbenchmark(
    old = quanteda3::tokens_group(toks),
    new = tokens_group(xtoks),
    times = 10
)

profvis::profvis(
    dfm(xtoks)
)

profvis::profvis(
    tokens_select(as.tokens_xptr(xtoks), stopwords(), padding = TRUE)
)

profvis::profvis(
    tokens_select(toks, stopwords(), padding = TRUE)
)

lis <- as.list(toks)
microbenchmark::microbenchmark(
    quanteda:::cpp_serialize(lis),
    quanteda:::serialize_tokens(lis),
    times = 10
)
