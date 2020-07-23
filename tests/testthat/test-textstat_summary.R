test_that("textstat_summary method works", {
    corp <- data_corpus_inaugural[10:15]
    toks <- tokens_tolower(tokens(corp))
    dfmt <- dfm(toks)

    col_summ <- c("document", "chars", "sents", "tokens", "types",
                  "puncts", "numbers", "symbols", "urls",
                  "tags", "emojis")

    # corpus
    summ_corp <- textstat_summary(corp, cache = FALSE)
    expect_equal(
        summ_corp$sents,
        unname(ntoken(tokens(corp, what = "sentence")))
    )
    expect_equal(
        names(summ_corp),
        col_summ
    )

    # tokens
    summ_toks <- textstat_summary(toks, cache = FALSE)
    expect_equal(
        summ_toks$puncts,
        unname(ntoken(tokens_select(toks, quanteda:::removals_regex(punct = TRUE)[[1]],
                                    valuetype = "regex")))
    )
    expect_equal(
        summ_toks$numbers,
        unname(ntoken(tokens_select(toks, quanteda:::removals_regex(numbers = TRUE)[[1]],
                                    valuetype = "regex")))
    )
    expect_equal(
        summ_toks$tokens,
        unname(ntoken(toks))
    )
    expect_equal(
        summ_toks$types,
        unname(ntype(toks))
    )
    expect_equal(
        summ_toks$sents,
        rep(NA, ndoc(toks))
    )
    expect_equal(
        names(summ_toks),
        col_summ
    )

    # dfm
    summ_dfm <- textstat_summary(dfmt, cache = FALSE)
    expect_equal(
        summ_dfm$puncts,
        unname(ntoken(dfm_select(dfmt, quanteda:::removals_regex(punct = TRUE)[[1]],
                                 valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$numbers,
        unname(ntoken(dfm_select(dfmt, quanteda:::removals_regex(numbers = TRUE)[[1]],
                                 valuetype = "regex")))
    )
    expect_equal(
        summ_dfm$tokens,
        unname(ntoken(dfmt))
    )
    expect_equal(
        summ_dfm$types,
        unname(ntype(dfmt))
    )
    expect_equal(
        summ_dfm$sents,
        rep(NA, ndoc(dfmt))
    )
    expect_equal(
        names(summ_dfm),
        col_summ
    )
})

test_that("summary chache counts hashtag and emoji correctly", {
    skip_on_os("solaris")
    txt <- c("£ € 👏 Rock on❗ 💪️🎸",
             "Hi @qi #quanteda https://quanteda.io")
    toks <- tokens(txt)
    summ <- textstat_summary(toks, cache = FALSE)
    expect_identical(summ$tokens, c(8L, 4L))
    expect_identical(summ$tags, c(0L, 2L))
    expect_identical(summ$emojis, c(4L, 0L))
    expect_identical(summ$urls, c(0L, 1L))
})

test_that("textstat_summary chaching is working", {
    skip_on_os("solaris")
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
    skip_on_os("solaris")
    corp <- data_corpus_inaugural[10:15]
    summ_corp1 <- textstat_summary(corp, cache = TRUE, tolower = FALSE, remove_punct = FALSE)
    summ_corp2 <- textstat_summary(corp, cache = TRUE, tolower = TRUE, remove_punct = FALSE)
    summ_corp3 <- textstat_summary(corp, cache = TRUE, tolower = TRUE, remove_punct = TRUE)
    expect_true(all(summ_corp1$types > summ_corp2$types))
    expect_true(all(summ_corp2$types > summ_corp3$types))

    toks <- tokens(corp)
    expect_identical(attr(toks, "meta")$object$summary$data, summ_corp3)
    summ_toks1 <- textstat_summary(toks, cache = TRUE, tolower = FALSE, remove_punct = FALSE)
    summ_toks2 <- textstat_summary(toks, cache = TRUE, tolower = TRUE, remove_punct = FALSE)
    summ_toks3 <- textstat_summary(toks, cache = TRUE, tolower = TRUE, remove_punct = TRUE)
    expect_true(all(summ_toks1$types > summ_toks2$types))
    expect_true(all(summ_toks2$types > summ_toks3$types))
    toks <- tokens_remove(toks, stopwords("en"))
    summ_toks4 <- textstat_summary(toks, cache = TRUE)
    expect_true(all(summ_toks3$types > summ_toks4$types))

    dfmt <- dfm(toks, tolower = FALSE)
    expect_identical(dfmt@meta$object$summary$data, summ_toks4)
    summ_dfm1 <- textstat_summary(dfmt, cache = TRUE, tolower = FALSE, stem = FALSE)
    summ_dfm2 <- textstat_summary(dfmt, cache = TRUE, tolower = TRUE, stem = FALSE)
    summ_dfm3 <- textstat_summary(dfmt, cache = TRUE, tolower = TRUE, stem = TRUE)
    expect_true(all(summ_dfm1$types > summ_dfm2$types))
    expect_true(all(summ_dfm2$types > summ_dfm3$types))
    dfmt <- dfm_remove(dfmt, "^[A-Z]", valuetype = "regex")
    summ_dfm4 <- textstat_summary(dfmt, cache = TRUE)
    expect_true(all(summ_dfm3$types > summ_dfm4$types))
})
