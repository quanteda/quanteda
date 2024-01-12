test_that("dfm defunct arguments generate correct errors", {
    txt <- data_char_ukimmig2010
    toks <- tokens(txt)
    
    expect_error(
        dfm(toks, stem = TRUE),
        "The `stem` argument of `dfm\\(\\)` was deprecated in quanteda 3\\.0.*defunct\\."
    )
    expect_error(
        dfm(toks, remove = "and"),
        "The `remove` argument of `dfm\\(\\)` was deprecated in quanteda 3\\.0.*defunct\\."
    )
    expect_error(
        dfm(toks, dictionary = data_dictionary_LSD2015),
        "The `dictionary` argument of `dfm\\(\\)` was deprecated in quanteda 3\\.0.*defunct\\."
    )
    expect_error(
        dfm(toks, thesaurus = data_dictionary_LSD2015),
        "Please use `dfm_lookup\\(\\.\\.\\., exclusive = FALSE\\)` instead\\."
    )
    expect_error(
        dfm(toks, valuetype = "regex"),
        "The `valuetype` argument of `dfm\\(\\)` was deprecated in quanteda.*3\\.0.*defunct\\.",
    )
    expect_error(
        dfm(toks, case_insensitive = FALSE),
        "The `case_insensitive` argument of `dfm\\(\\)` was deprecated in quanteda.*3\\.0.*defunct\\.",
    )
    expect_error(
        dfm(toks, groups = 1),
        "The `groups` argument of `dfm\\(\\)` was deprecated in quanteda 3\\.0.*defunct\\."
    )
})

test_that("as.data.frame for dfm objects", {
    lifecycle::expect_defunct(
        as.data.frame(data_dfm_lbgexample[, 1:5])
    )
})

test_that("dfm message works correctly on defunct methods", {
    expect_error(
        dfm("dfm.character was defuncted in v4"),
        "`dfm.character()` was deprecated in quanteda 3.0 and is now defunct.",
        fixed = TRUE
    )
    expect_error(
        dfm(corpus("dfm.corpus was defuncted in v4")),
        "`dfm.corpus()` was deprecated in quanteda 3.0 and is now defunct.",
        fixed = TRUE
    )
})

test_that("kwic deprecations work as expected", {
    txt <- "A b c d e."
    expect_error(
        kwic(txt, "c", window = 1),
        "`kwic.character\\(\\)` was deprecated in quanteda 3\\.0"
    )
    expect_error(
        kwic(corpus(txt), "c", window = 1),
        "`kwic.corpus\\(\\)` was deprecated in quanteda 3\\.0"
    )
})

test_that("texts produces a defunct message", {
    expect_error(
        texts(data_corpus_inaugural),
        "`texts\\(\\)` was deprecated in quanteda 3\\.0 and is now.*defunct\\."
    )
    expect_error(
        texts(data_corpus_inaugural, groups = Party),
        "`texts\\(x, groups = \\.\\.\\.\\)` was deprecated in quanteda 3\\.0.*defunct\\."
    )
})

test_that("char_ngrams produces a deprecated message", {
    txt <- c('insurgents','killed', 'in', 'ongoing', 'fighting')
    lifecycle::expect_deprecated(
        char_ngrams(txt),
        "char_ngrams() was deprecated in quanteda 4.0",
        fixed = TRUE
    )
})

test_that("nsentence produces a deprecated message", {
    skip_if_not_installed("rlang")
    rlang::local_options(lifecycle_verbosity = "warning")
    lifecycle::expect_deprecated(
        nsentence("This is one.  And two."),
        "`nsentence()` was deprecated in quanteda 4.0",
        fixed = TRUE
    )
    expect_error(
        texts(data_corpus_inaugural, groups = Party),
        "`texts\\(x, groups = \\.\\.\\.\\)` was deprecated in quanteda 3.0.*defunct\\.",
    )
    rlang::local_options(lifecycle_verbosity = "default")
})
