test_that("summary method works", {
    
    corp <- data_corpus_inaugural
    toks <- tokens(corp)
    dfmt <- dfm(toks)
    
    # corpus
    summ_corp <- summary(corp, cache = FALSE)
    expect_equal(
        summ_corp$punct,
        unname(ntoken(dfm_select(dfmt, "[\\p{P}]", valuetype = "regex")))
    )
    expect_equal(
        summ_corp$number,
        unname(ntoken(dfm_select(dfmt, "[\\p{N}]", valuetype = "regex")))
    )
    expect_equal(
        summ_corp$n_sent,
        unname(ntoken(tokens(corp, what = "sentence")))
    )
    expect_equal(
        summ_corp$is_dup,
        rep(FALSE, ndoc(corp))
    )
    
    # tokens
    summ_toks <- summary(toks, cache = FALSE)
    expect_equal(
        summ_toks$punct,
        unname(ntoken(dfm_select(dfmt, "[\\p{P}]", valuetype = "regex")))
    )
    expect_equal(
        summ_toks$number,
        unname(ntoken(dfm_select(dfmt, "[\\p{N}]", valuetype = "regex")))
    )
    expect_equal(
        summ_toks$n_sent,
        rep(NA, ndoc(toks))
    )
    expect_equal(
        summ_toks$is_dup,
        rep(FALSE, ndoc(toks))
    )
    
    # dfm
    summ_dfm <- summary(dfmt, cache = FALSE)
    expect_equal(
        summ_dfm$punct,
        unname(ntoken(dfm_select(dfmt, "[\\p{P}]", valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$number,
        unname(ntoken(dfm_select(dfmt, "[\\p{N}]", valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$n_sent,
        rep(NA, ndoc(dfmt))
    )
    expect_equal(
        summ_dfm$is_dup,
        rep(NA, ndoc(dfmt))
    )
    
})


test_that("summary chaching is working", {
    
    corp <- data_corpus_inaugural
    summ_corp <- summary(corp, cache = TRUE)
    expect_identical(meta(corp, type = "object")$summary$data, summ_corp)
    summ_corp <- summary(corp, cache = FALSE)
    quanteda:::clear_cache(corp, "summary")
    expect_identical(meta(corp, type = "object")$summary, list())
    expect_equal(nrow(summary(head(corp, 10), cache = TRUE)), 10)
    
    toks <- tokens(corp)
    summ_toks <- summary(toks, cache = TRUE)
    expect_identical(meta(toks, type = "object")$summary$data, summ_toks)
    summ_toks <- summary(toks, cache = FALSE)
    expect_identical(meta(toks, type = "object")$summary, list())
    expect_equal(nrow(summary(head(toks, 10), cache = TRUE)), 10)
    
    dfmt <- dfm(toks)
    summ_dfm <- summary(dfmt, cache = TRUE)
    expect_identical(meta(dfmt, type = "object")$summary$data, summ_dfm)
    summ_dfm <- summary(dfmt, cache = FALSE)
    expect_identical(meta(dfmt, type = "object")$summary, list())
    expect_equal(nrow(summary(head(dfmt, 10), cache = TRUE)), 10)
})
