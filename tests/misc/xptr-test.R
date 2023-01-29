require(quanteda)
data(data_corpus_sotu, package = "quanteda.corpora")
corp <- as.corpus(data_corpus_sotu)
toks <- tokens(corp)
xtoks <- as.externalptr(toks)
class(toks)
class(xtoks)
identical(types(toks), types(xtoks))

toks2 <- tokens_remove(toks, stopwords(), padding = TRUE) %>% 
    tokens_select(data_dictionary_LSD2015, padding = TRUE)
xtoks2 <- tokens_remove(xtoks, stopwords(), padding = TRUE) %>% 
    tokens_select(data_dictionary_LSD2015, padding = TRUE)

identical(as.list(toks2), as.list(as.tokens(xtoks2)))

microbenchmark::microbenchmark(
    tokens_remove(toks, stopwords(), padding = TRUE),
    tokens_remove(as.externalptr(toks), stopwords(), padding = TRUE),
    times = 10
)

microbenchmark::microbenchmark(
    tokens_select(toks, data_dictionary_LSD2015, padding = TRUE),
    tokens_select(as.externalptr(toks), data_dictionary_LSD2015, padding = TRUE),
    times = 10
)

microbenchmark::microbenchmark(
    tokens_remove(toks, stopwords(), padding = TRUE),
    tokens_remove(xtoks, stopwords(), padding = TRUE),
    times = 10
)

# simulate tokens internal -----------------------------
toks_raw <- tokens(corp, remove_separators = FALSE, remove_punct = FALSE, 
                   remove_symbols = FALSE, remove_numbers = FALSE, remove_url = FALSE)
removals <- quanteda:::removals_regex(separators = TRUE, punct = TRUE,
                                      symbols = TRUE, numbers = TRUE, url = TRUE)
microbenchmark::microbenchmark(
    lis0 = tokens_remove(toks_raw, paste(unlist(removals), collapse = "|"),
                         valuetype = "regex",  padding = TRUE),
    lis5 = toks_raw %>% 
        tokens_remove(removals[[1]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[2]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[3]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[4]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[5]], valuetype = "regex", padding = TRUE),
    xptr0 = tokens_remove(as.externalptr(toks_raw), paste(unlist(removals), collapse = "|"),
                         valuetype = "regex",  padding = TRUE),
    xptr1 = tokens_remove(as.externalptr(toks_raw), paste(unlist(removals), collapse = "|"),
                          valuetype = "regex",  padding = TRUE) %>% 
        as.tokens(),
    xptr5 = as.externalptr(toks_raw) %>% 
        tokens_remove(removals[[1]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[2]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[3]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[4]], valuetype = "regex", padding = TRUE) %>% 
        tokens_remove(removals[[5]], valuetype = "regex", padding = TRUE),
    times = 10
)


#--------------------------

type <- types(toks)
ids <- object2id(data_dictionary_LSD2015, type)
startpos <- rep(1, length.out = ndoc(toks))
endpos <- rep(-1, length.out = ndoc(toks))

xtoks <- quanteda:::qatd_cpp_as_xptr(toks, type)
class(xtoks)

tokens_select(xtoks, data_dictionary_LSD2015)

tokens_select_old <- function(x) {
    quanteda:::qatd_cpp_tokens_select(x, type, ids, 1, TRUE, 0, 0, startpos, endpos)
}

tokens_select_new <- function(x) {
    is_xptr <- identical(class(x), "externalptr")
    if (!is_xptr)
        x <- quanteda:::qatd_cpp_as_xptr(x, type)
    result <- quanteda:::qatd_cpp_tokens_select_xptr(x, ids, 1, TRUE, 0, 0, startpos, endpos)
    if (!is_xptr)
        result <- quanteda:::qatd_cpp_as_list(result)
    return(result)
}

toks1 <- tokens_select_old(toks)
toks2 <- tokens_select_new(toks)
toks3 <- tokens_select_new(xtoks)
class(toks1)
class(toks2)
class(toks3)
identical(unclass(toks1), unclass(toks2))
identical(unclass(toks1), 
          unclass(quanteda:::qatd_cpp_as_list(toks3)))
identical(unclass(toks2), 
          unclass(quanteda:::qatd_cpp_as_list(toks3)))

microbenchmark::microbenchmark(
    tokens_select_old(toks),
    tokens_select_new(toks), 
    tokens_select_new(xtoks),
    times = 10
)

microbenchmark::microbenchmark(
    toks %>% tokens_select_old() %>% tokens_select_old() %>% tokens_select_old(),
    toks %>% tokens_select_new() %>% tokens_select_new() %>% tokens_select_new(),
    xtoks %>% tokens_select_new() %>% tokens_select_new() %>% tokens_select_new(),
    times = 10
)

