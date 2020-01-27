library("quanteda")
data(data_corpus_sotu, package = "quanteda.corpora")
data_corpus_sotu <- as.corpus(data_corpus_sotu)

microbenchmark(
    word = tokens(data_corpus_sotu, what = "word"),
    word1 = tokens(data_corpus_sotu, what = "word1"),
    tokenizers = as.tokens(tokenizers::tokenize_words(texts(data_corpus_sotu),
                                                      lowercase = FALSE,
                                                      strip_punct = FALSE)),
    times = 10
)



x <- data_corpus_sotu

txt <- c(doc1 = "Tweet https://quanteda.io using @quantedainit and #rstats.",
         doc2 = "The £1,000,000 question.",
         doc3 = "毎日 #quanteda を使用してください！",
         doc4 = "Line 1.\nLine2\n\nLine3.",
         doc5 = "?",
         doc6 = "Self-aware machines! \U0001f600")

library("microbenchmark")


microbenchmark(
    word = tokens(x, what = "word"),
    word1 = tokens(x, what = "word1"),
    faster = tokens(x, what = "fasterword"),
    fastest = tokens(x, what = "fastestword"),
    times = 3, unit = "relative"
)


# 
# 
# microbenchmark(regex = stri_detect_regex(x, "^((https{0,1}|s{0,1}ftp)://)|(\\w+@\\w+)"),
#                fixed = stri_detect_fixed(x, "http"), times = 10, unit = "relative")


toks <- tokens(data_corpus_sotu)
microbenchmark(
  fixed = tokens_split(toks, "@", valuetype = "fixed", remove_separator = F) %>% tokens_split("#", valuetype = "fixed", remove_separator = F),
  regex = tokens_split(toks, "[@#]", valuetype = "regex", remove_separator = F),
  times = 2, unit = "relative"
)

microbenchmark(
    word = tokens(x, what = "word"),
    word1 = tokens(x, what = "word1"),
    times = 10
)
