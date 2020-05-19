test_that("textstat_summary method works", {
    
    corp <- data_corpus_inaugural[10:15]
    toks <- tokens_tolower(tokens(corp))
    dfmt <- dfm(toks)
    
    # corpus
    summ_corp <- textstat_summary(corp, cache = FALSE)
    expect_equal(
        summ_corp$n_sent,
        unname(ntoken(tokens(corp, what = "sentence")))
    )
    expect_equal(
        summ_corp$is_dup,
        rep(FALSE, ndoc(corp))
    )
    expect_equal(
        names(summ_corp),
        c("document", "n_token", "n_type", "n_sent", "number", 
          "punct", "symbol", "any", "noise", "is_dup")
    )
    
    # tokens
    summ_toks <- textstat_summary(toks, cache = FALSE)
    expect_equal(
        summ_toks$punct,
        unname(ntoken(tokens_select(toks, "[\\p{P}]", valuetype = "regex")))
    )
    expect_equal(
        summ_toks$number,
        unname(ntoken(tokens_select(toks, "[\\p{N}]", valuetype = "regex")))
    )
    expect_equal(
        summ_toks$n_token,
        unname(ntoken(toks))
    )
    expect_equal(
        summ_toks$n_type,
        unname(ntype(toks))
    )
    expect_equal(
        summ_toks$n_sent,
        rep(NA, ndoc(toks))
    )
    expect_equal(
        summ_toks$is_dup,
        rep(FALSE, ndoc(toks))
    )
    expect_equal(
        names(summ_toks),
        c("document", "n_token", "n_type", "n_sent", "number", 
          "punct", "symbol", "any", "noise", "is_dup")
    )
    
    # dfm
    summ_dfm <- textstat_summary(dfmt, cache = FALSE)
    expect_equal(
        summ_dfm$punct,
        unname(ntoken(dfm_select(dfmt, "[\\p{P}]", valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$number,
        unname(ntoken(dfm_select(dfmt, "[\\p{N}]", valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$n_token,
        unname(ntoken(dfmt))
    )
    expect_equal(
        summ_dfm$n_type,
        unname(ntype(dfmt))
    )
    expect_equal(
        summ_dfm$n_sent,
        rep(NA, ndoc(dfmt))
    )
    expect_equal(
        summ_dfm$is_dup,
        rep(NA, ndoc(dfmt))
    )
    expect_equal(
        names(summ_dfm),
        c("document", "n_token", "n_type", "n_sent", "number", 
          "punct", "symbol", "any", "noise", "is_dup")
    )
})


test_that("textstat_summary chaching is working", {
    
    corp <- data_corpus_inaugural[10:15]
    summ_corp <- textstat_summary(corp, cache = TRUE)
    expect_identical(meta(corp, type = "object")$summary$data, summ_corp)
    summ_corp <- textstat_summary(corp, cache = FALSE)
    quanteda:::clear_cache(corp, "summary")
    expect_identical(meta(corp, type = "object")$summary, list())
    expect_equal(nrow(textstat_summary(head(corp, 3), cache = TRUE)), 3)
    
    toks <- tokens(corp)
    summ_toks <- textstat_summary(toks, cache = TRUE)
    expect_identical(meta(toks, type = "object")$summary$data, summ_toks)
    summ_toks <- textstat_summary(toks, cache = FALSE)
    expect_identical(meta(toks, type = "object")$summary, list())
    expect_equal(nrow(textstat_summary(head(toks, 3), cache = TRUE)), 3)
    
    dfmt <- dfm(toks)
    summ_dfm <- textstat_summary(dfmt, cache = TRUE)
    expect_identical(meta(dfmt, type = "object")$summary$data, summ_dfm)
    summ_dfm <- textstat_summary(dfmt, cache = FALSE)
    expect_identical(meta(dfmt, type = "object")$summary, list())
    expect_equal(nrow(textstat_summary(head(dfmt, 3), cache = TRUE)), 3)
})


test_that("summary chache is updated", {
    
    corp <- data_corpus_inaugural[10:15]
    summ_corp1 <- textstat_summary(corp, cache = TRUE, tolower = FALSE, remove_punct = FALSE)
    summ_corp2 <- textstat_summary(corp, cache = TRUE, tolower = TRUE, remove_punct = FALSE)
    summ_corp3 <- textstat_summary(corp, cache = TRUE, tolower = TRUE, remove_punct = TRUE)
    expect_true(all(summ_corp1$n_type > summ_corp2$n_type))
    expect_true(all(summ_corp2$n_type > summ_corp3$n_type))
    
    toks <- tokens(corp)
    expect_identical(attr(toks, "meta")$object$summary$data, summ_corp3)
    summ_toks1 <- textstat_summary(toks, cache = TRUE, tolower = FALSE, remove_punct = FALSE)
    summ_toks2 <- textstat_summary(toks, cache = TRUE, tolower = TRUE, remove_punct = FALSE)
    summ_toks3 <- textstat_summary(toks, cache = TRUE, tolower = TRUE, remove_punct = TRUE)
    expect_true(all(summ_toks1$n_type > summ_toks2$n_type))
    expect_true(all(summ_toks2$n_type > summ_toks3$n_type))
    toks <- tokens_remove(toks, stopwords("en"))
    summ_toks4 <- textstat_summary(toks, cache = TRUE)
    expect_true(all(summ_toks3$n_type > summ_toks4$n_type))
    
    dfmt <- dfm(toks, tolower = FALSE)
    expect_identical(dfmt@meta$object$summary$data, summ_toks4)
    summ_dfm1 <- textstat_summary(dfmt, cache = TRUE, tolower = FALSE, stem = FALSE)
    summ_dfm2 <- textstat_summary(dfmt, cache = TRUE, tolower = TRUE, stem = FALSE)
    summ_dfm3 <- textstat_summary(dfmt, cache = TRUE, tolower = TRUE, stem = TRUE)
    expect_true(all(summ_dfm1$n_type > summ_dfm2$n_type))
    expect_true(all(summ_dfm2$n_type > summ_dfm3$n_type))
    dfmt <- dfm_remove(dfmt, "^[A-Z]", valuetype = "regex")
    summ_dfm4 <- textstat_summary(dfmt, cache = TRUE)
    expect_true(all(summ_dfm3$n_type > summ_dfm4$n_type))
})
