txt <- texts(data_corpus_dailnoconf1991)
txt <- c(txt, txt, txt)
names(txt) <- NULL

microbenchmark::microbenchmark(
    boun = stri_split_boundaries(txt, type = "word",
                                     # this is what kills currency symbols, Twitter tags, URLs
                                     skip_word_none = FALSE,
                                     # but does not remove 4u, 2day, etc.
                                     skip_word_number = FALSE),
    tokenize_regex = tokenizers::tokenize_regex(txt, "[\\p{Z}\\p{C}]+"),
    regex = stri_split_regex(txt, "[\\p{Z}\\p{C}]+"),
    charc = stri_split_regex(txt, "[\\p{Z}\\p{C}]"),
    tokenizers = tokenizers::tokenize_words(txt, lowercase = FALSE, strip_punct = FALSE),
    fixed = stri_split_fixed(txt, " "),
    times = 20, unit = "relative"
)

microbenchmark::microbenchmark(
    #boun = stri_split_boundaries(txt, type = "line_break"),
    charc = stri_split_regex(txt, "[\\p{C}]"),
    tokenizers = tokenizers::tokenize_lines(txt),
    # fixed = stri_split_fixed(txt, " "),
    times = 20, unit = "relative"
)

txt2 <- "One\n\nTwo   Three."
names(txt) <- NULL
microbenchmark(
    fastest = tokens(txt, what = "fastestword"),
    fastestplus = tokens(txt, what = "fastestword") %>% 
        tokens_split(separator = "[\\p{Z}\\p{C}]+", valuetype = "regex"),
    faster = tokens(txt, what = "fasterword"),
    times = 20, unit = "relative"
)


tokens(txt2, what = "fastestword") %>% 
    tokens_split(separator = "[\\p{Z}\\p{C}]+", valuetype = "regex") %>%
    print(1)

chunked <- chunk_text(mobydick, chunk_size = 100)
length(chunked)
chunked[1:3]

song <- list(paste0("How many roads must a man walk down\n",
                    "Before you call him a man?"),
             paste0("How many seas must a white dove sail\n",
                    "Before she sleeps in the sand?\n"),
             paste0("How many times must the cannonballs fly\n",
                    "Before they're forever banned?\n"),
             "The answer, my friend, is blowin' in the wind.",
             "The answer is blowin' in the wind.")
tokenize_ptb(song)

txt <- c(d1 = "re-energize ξ μ one\n\ntwo   #hashtag 😄😄😄, 4ever."
 https://example.com?p=123
s)tri_split_lines(txt)
tokenize_lines(txt)

tokenize_words(txt, strip_punct = FALSE, strip_numeric = FALSE, lowercase = FALSE)
tokenize_tweets(txt, lowercase = FALSE, strip_punct = FALSE)
