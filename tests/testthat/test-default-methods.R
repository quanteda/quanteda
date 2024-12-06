test_that("test default corpus* methods", {
    expect_error(
        corpus(TRUE),
        "corpus\\(\\) only works on.*character.*corpus.*objects"
    )
    expect_error(
        corpus_group(TRUE),
        "corpus_group\\(\\) only works on corpus objects"
    )
    expect_error(
        as.corpus(c(1, 2, 3)),
        "as.corpus\\(\\) only works on corpus objects"
    )
    expect_error(
        corpus_reshape(1),
        "corpus_reshape\\(\\) only works on corpus objects"
    )
    expect_error(
        corpus_sample(1),
        "corpus_sample\\(\\) only works on corpus objects"
    )
    expect_error(
        corpus_segment(1),
        "corpus_segment\\(\\) only works on corpus objects"
    )
    expect_error(
        corpus_subset(1),
        "corpus_subset\\(\\) only works on corpus objects"
    )
})

test_that("test detault n-methods", {
    expect_error(
        ndoc(TRUE),
        "ndoc\\(\\) only works on corpus.*tokens_xptr objects"
    )
    expect_error(
        nfeat(TRUE),
        "nfeat\\(\\) only works on dfm.*objects"
    )
    expect_error(
        ntoken(TRUE),
        "ntoken\\(\\) only works on character.*tokens_xptr objects"
    )
    expect_error(
        ntype(TRUE),
        "ntype\\(\\) only works on character.*tokens_xptr objects"
    )
})

test_that("test detault char_* methods", {
    expect_error(
        char_ngrams(1),
        "char_ngrams\\(\\) only works on character objects"
    )
    expect_error(
        char_segment(1),
        "char_segment\\(\\) only works on character objects"
    )
    expect_error(
        char_tolower(1),
        "char_tolower\\(\\) only works on character objects"
    )
    expect_error(
        char_toupper(1),
        "char_toupper\\(\\) only works on character objects"
    )
    expect_error(
        char_wordstem(1),
        "char_wordstem\\(\\) only works on character objects"
    )
})

test_that("test detault fcm* methods", {
    expect_error(
        fcm(0),
        "fcm\\(\\) only works on character.*tokens_xptr objects"
    )
    expect_error(
        fcm_compress(1),
        "fcm_compress\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_keep(1),
        "fcm_select\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_remove(1),
        "fcm_select\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_select(1),
        "fcm_select\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_sort(1),
        "fcm_sort\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_tolower(1),
        "fcm_tolower\\(\\) only works on fcm objects"
    )
    expect_error(
        fcm_toupper(1),
        "fcm_toupper\\(\\) only works on fcm objects"
    )
})

test_that("test default docvars methods", {
    expect_error(
        docvars(0),
        "docvars\\(\\) only works on corpus.*tokens objects"
    )
    expect_error(
        docvars(data_char_sampletext) <- "X",
        "docvars<-\\(\\) only works on corpus.*tokens objects"
    )
})

test_that("kwic default works", {
    expect_error(
        kwic(TRUE),
        "kwic\\(\\) only works on character, corpus, tokens, tokens_xptr objects"
    )
})

test_that("phrase default works", {
    expect_error(
        phrase(TRUE),
        "phrase\\(\\) only works on character, collocations, dictionary2, list, tokens objects"
    )
    
    expect_error(
        as.phrase(TRUE),
        "phrase\\(\\) only works on collocations, dictionary2, list, tokens objects"
    )
})

test_that("types defaults work", {
    expect_error(
        types(TRUE),
        "types\\(\\) only works on tokens, tokens_xptr objects"
    )
    # expect_error(
    #     quanteda:::types(data_char_sampletext) <- c("a", "b"),
    #     "types<-\\(\\) only works on tokens objects"
    # )
})

test_that("concatenator defaults work", {
    expect_error(
        concatenator(TRUE),
        "concatenator\\(\\) only works on tokens, tokens_xptr objects"
    )
})

test_that("concat defaults work", {
    expect_error(
        concat(TRUE),
        "concat\\(\\) only works on tokens, tokens_xptr objects"
    )
})

test_that("test new bootstrap_dfm methods", {
    expect_error(
        bootstrap_dfm(TRUE),
        "bootstrap_dfm\\(\\) only works on dfm objects"
    )
})

test_that("test new convert methods", {
    expect_error(
        convert(TRUE),
        "convert\\(\\) only works on .*dfm.*objects"
    )
})

test_that("test new dfm methods", {
    expect_error(
        dfm(TRUE),
        "dfm\\(\\) only works on dfm, tokens, tokens_xptr objects"
    )
    expect_error(
        dfm_compress(TRUE),
        "dfm_compress\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_group(TRUE),
        "dfm_group\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_keep(TRUE),
        "dfm_select\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_match(TRUE),
        "dfm_match\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_lookup(TRUE),
        "dfm_lookup\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_remove(TRUE),
        "dfm_select\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_sample(TRUE),
        "dfm_sample\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_replace(TRUE),
        "dfm_replace\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_select(TRUE),
        "dfm_select\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_smooth(TRUE),
        "dfm_smooth\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_sort(TRUE),
        "dfm_sort\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_subset(TRUE),
        "dfm_subset\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_tolower(TRUE),
        "dfm_tolower\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_toupper(TRUE),
        "dfm_toupper\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_trim(TRUE),
        "dfm_trim\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_weight(TRUE),
        "dfm_weight\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_wordstem(TRUE),
        "dfm_wordstem\\(\\) only works on dfm objects"
    )
    expect_error(
        dfm_tfidf(TRUE),
        "dfm_tfidf\\(\\) only works on dfm objects"
    )
    expect_error(
        docfreq(TRUE),
        "docfreq\\(\\) only works on dfm objects"
    )
})

test_that("test token default methods", {
    expect_error(
        as.tokens(c(1, 2, 3)),
        "as.tokens\\(\\) only works on list, spacyr_parsed, tokens, tokens_xptr objects"
    )
    expect_error(
        tokens(TRUE),
        "tokens\\(\\) only works on character, corpus, list, tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_group(TRUE),
        "tokens_group\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_subset(TRUE),
        "tokens_subset\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_compound(TRUE),
        "tokens_compound\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_split(TRUE),
        "tokens_split\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_keep(TRUE),
        "tokens_select\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_segment(TRUE),
        "tokens_segment\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_chunk(TRUE),
        "tokens_chunk\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_lookup(TRUE),
        "tokens_lookup\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_sample(TRUE),
        "tokens_sample\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_ngrams(TRUE),
        "tokens_ngrams\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_remove(TRUE),
        "tokens_select\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_replace(TRUE),
        "tokens_replace\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_select(TRUE),
        "tokens_select\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_skipgrams(TRUE),
        "tokens_skipgrams\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_tolower(TRUE),
        "tokens_tolower\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_toupper(TRUE),
        "tokens_toupper\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_wordstem(TRUE),
        "tokens_wordstem\\(\\) only works on tokens, tokens_xptr objects"
    )
    expect_error(
        tokens_group(TRUE),
        "tokens_group\\(\\) only works on tokens, tokens_xptr objects"
    )
})

test_that("test token_xptr default methods", {
    expect_error(
        as.tokens_xptr(c(1, 2, 3)),
        "as.tokens_xptr() only works on tokens, tokens_xptr objects.",
        fixed = TRUE
    )
})

test_that("test new as.dfm methods", {
    expect_error(
        as.dfm(TRUE),
        "as\\.dfm\\(\\) only works on.*data\\.frame.*dfm.*matrix.*objects"
    )
})

test_that("test sparsity default", {
    expect_error(
        sparsity(TRUE),
        "sparsity\\(\\) only works on dfm objects"
    )
})

test_that("test topfeatures default", {
    expect_error(
        topfeatures(TRUE),
        "topfeatures\\(\\) only works on dfm objects"
    )
})

test_that("test new docnames methods", {
    expect_error(
        docnames(0),
        "docnames\\(\\) only works on.*corpus.*tokens objects"
    )
})

test_that("test new docnames<- methods", {
    expect_error(
        docnames(data_char_sampletext) <- "X",
        "docnames<-\\(\\) only works on.*corpus.*tokens objects"
    )
})

test_that("test docid methods", {
    expect_error(
        docid(0),
        "docid() only works on corpus, dfm, tokens objects.", fixed = TRUE
    )
})

test_that("test segid methods", {
  expect_error(
    segid(0),
    "segid() only works on corpus, dfm, tokens objects.", fixed = TRUE
  )
})

test_that("friendly_class_undefined_message for featfreq()", {
    expect_error(
        featfreq(tokens(data_char_sampletext)),
        "featfreq\\(\\) only works on dfm objects"
    )
})

test_that("friendly_class_undefined_message for char_select()", {
    expect_error(
        char_select(tokens(data_char_sampletext)),
        "char_select() only works on character objects",
        fixed = TRUE
    )
})

