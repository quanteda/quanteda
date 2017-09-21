library(quanteda)
quanteda_options(threads = 7)

load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
corp <- data_corpus_guardian

load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian

microbenchmark::microbenchmark(
    tokens_segment(toks, '^\\p{Pe}$', valuetype = 'regex', extract_pattern = TRUE),
    tokens_segment(toks, '^\\p{Pe}$', valuetype = 'regex', extract_pattern = FALSE),
    times = 5
)

microbenchmark::microbenchmark(
    tokens_segment(toks, '^\\p{Pe}$', valuetype = 'regex', pattern_position = 'after',
                   use_docvars = FALSE),
    tokens_segment(toks, '^\\p{Pe}$', valuetype = 'regex', pattern_position = 'after',
                   use_docvars = TRUE),
    times = 5
)

microbenchmark::microbenchmark(
    corpus = corpus_segment(corp, '\\p{P}', valuetype = 'regex', extract_pattern = TRUE),
    token = tokens_segment(toks, '^\\p{P}$', valuetype = 'regex', extract_pattern = TRUE),
    times = 5
)

profvis::profvis(tokens_segment(toks, '^\\p{P}$', valuetype = 'regex', pattern_position = 'after'))
