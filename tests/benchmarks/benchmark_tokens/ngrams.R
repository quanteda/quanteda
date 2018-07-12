library(quanteda)
quanteda_options(threads = 7)

corp <- readRDS("/home/kohei/Documents/Brexit/Data/data_corpus_guardian.RDS")
docvars(corp, 'year') <- format(docvars(corp, 'date'), '%Y')
corp <- corpus_reshape(corp)
txts <- texts(corpus_subset(corp, year == 2015))

microbenchmark::microbenchmark(
    tokenizers = tokenizers::tokenize_ngrams(txts, n = 2),
    quanteda = tokens_ngrams(as.tokens(tokenizers::tokenize_words(txts)), n = 2),
    unit='relative', times = 1
)

object.size(tokenizers::tokenize_ngrams(txts, n = 2), 'MB')
object.size(tokens_ngrams(as.tokens(tokenizers::tokenize_words(txts)), n = 2), 'MB')

microbenchmark::microbenchmark(
    tokenizers = tokenizers::tokenize_skip_ngrams(txts, n = 2, k = 1),
    quanteda = tokens_ngrams(as.tokens(tokenizers::tokenize_words(txts)), n = 2, skip = 1),
    unit='relative', times = 1
)

toks <- tokens(txts, removeSymbols = TRUE, removeNumbers = TRUE)

microbenchmark::microbenchmark(
    tokens_ngrams(toks, n=2),
    tokens_ngrams(toks, n=3),
    tokens_ngrams(toks, n=4),
    tokens_ngrams(toks, n=5),
    times=1
)

microbenchmark::microbenchmark(
    tokens_ngrams(toks, n=2), times=10
)

profvis::profvis(tokens_ngrams(toks, 2))
profvis::profvis(quanteda::tokens_ngrams(tokens(txts), n = 2))

