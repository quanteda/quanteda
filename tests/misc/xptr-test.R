require(quanteda)
data(data_corpus_sotu, package = "quanteda.corpora")
corp <- as.corpus(data_corpus_sotu)
toks <- tokens(corp)

type <- types(toks)
ids <- object2id(data_dictionary_LSD2015, type)
startpos <- rep(1, length.out = ndoc(toks))
endpos <- rep(-1, length.out = ndoc(toks))

xtoks <- quanteda:::qatd_cpp_as_xptr(toks, type)
class(xtoks)

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

