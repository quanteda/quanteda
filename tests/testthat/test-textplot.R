context("test plots.R")

pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test plot.kwic scale argument default", {

    sda <- kwic(texts(data_corpus_inaugural)[[1]], "american")
    sdp <- kwic(texts(data_corpus_inaugural)[[1]], "people")
    mda <- kwic(data_corpus_inaugural, "american")
    mdp <- kwic(data_corpus_inaugural, "people")

    # Single document, should be absolute
    p <- textplot_xray(sda)
    expect_equal(p$labels$x, "Token index")

    # Single document, multiple keywords, should be absolute
    p <- textplot_xray(sda, sdp)
    expect_equal(p$labels$x, "Token index")

    # Multiple documents, should be relative
    p <- textplot_xray(mda)
    expect_equal(p$labels$x, "Relative token index")

    # Multiple documents, multiple keywords, should be relative
    p <- textplot_xray(mda, mdp)
    expect_equal(p$labels$x, "Relative token index")

    # Explicit overrides
    p <- textplot_xray(sda, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(sda, sdp, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(mda, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(mda, mdp, scale = "absolute")
    expect_equal(p$labels$x, "Token index")

    p <- textplot_xray(sda, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(sda, sdp, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(mda, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(mda, mdp, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")


})


test_that("test plot.kwic facet order parameter", {

    p <- textplot_xray(kwic(data_corpus_inaugural, "american"), sort = TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )
    p <- textplot_xray(kwic(data_corpus_inaugural, "american"),
                       kwic(data_corpus_inaugural, "people"),
                       sort = TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

    # Default should be false
    p <- textplot_xray(kwic(data_corpus_inaugural[c(53:54, 1:2)], "american"),
                       kwic(data_corpus_inaugural[c(53:54, 1:2)], "people"))
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_false(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

})

# test_that("test plot.kwic keeps order of keywords passed", {
#     p <- textplot_xray(kwic(data_corpus_inaugural, "people"), kwic(data_corpus_inaugural, "american"), sort = TRUE)
#     keywords <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$keyword))
#     if (identical(keywords, character(0))) {
#         keywords <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$keyword))
#     }
#     expect_equal(
#         keywords,
#         c("people", "american")
#     )
# })

test_that("test textplot_wordcloud works for dfm objects", {
    mt <- dfm(data_corpus_inaugural[1:5])
    mt <- dfm_trim(mt, min_termfreq = 10)
    expect_silent(textplot_wordcloud(mt))
})

test_that("test textplot_wordcloud works for keyness objects", {
    tstat <- head(data_corpus_inaugural, 2) %>%
        dfm(remove_punct = TRUE, remove = stopwords("en")) %>%
        textstat_keyness(target = 1)
    expect_silent(textplot_wordcloud(tstat, max_words = 50))
    expect_silent(textplot_wordcloud(tstat, comparison = FALSE, max_words = 50))
})

test_that("test textplot_wordcloud comparison works", {
    skip_on_travis()
    skip_on_cran()
    skip_on_os("linux")
    testcorp <- corpus_reshape(corpus(data_char_sampletext))
    set.seed(1)
    docvars(testcorp, "label") <- sample(c("A", "B"), size = ndoc(testcorp), replace = TRUE)
    docnames(testcorp) <- paste0("text", 1:ndoc(testcorp))
    testdfm <- dfm(testcorp, remove = stopwords("english"))
    testdfm_grouped <- dfm(testcorp, remove = stopwords("english"), groups = "label")

    jpeg(filename = tempfile(".jpg"), width = 5000, height = 5000)
    expect_silent(
        textplot_wordcloud(testdfm_grouped, comparison = TRUE)
    )
    expect_silent(
        textplot_wordcloud(testdfm_grouped, random_order = FALSE)
    )
    expect_silent(
        textplot_wordcloud(testdfm_grouped, ordered_color = FALSE)
    )
    expect_error(
        textplot_wordcloud(dfm(data_corpus_inaugural[1:9]), comparison = TRUE),
        "Too many documents to plot comparison, use 8 or fewer documents"
    )
    
    dfmsmall <- dfm(data_corpus_inaugural[1:9], groups = "President", remove = stopwords("en"), remove_punct = TRUE) %>%
        dfm_trim(min_termfreq = 20)
    expect_silent(textplot_wordcloud(dfmsmall, comparison = TRUE))
    expect_silent(textplot_wordcloud(dfmsmall, color = 1:5))
    expect_warning(
        textplot_wordcloud(dfmsmall, scale = 1:4),
        "scale is deprecated"
    )
    expect_warning(
        textplot_wordcloud(dfmsmall, random.order = TRUE),
        "random.order is deprecated; use random_order instead"
    )
    expect_warning(
        textplot_wordcloud(dfmsmall, max.words = 10),
        "max.words is deprecated; use max_words instead"
    )
    
    dev.off()
    expect_error(
        textplot_wordcloud(testdfm, comparison = TRUE),
        "Too many documents to plot comparison, use 8 or fewer documents\\."
    )
})

test_that("test textplot_wordcloud raise deprecation message", {
    jpeg(filename = tempfile(".jpg"), width = 5000, height = 5000)
    mt <- dfm(data_corpus_inaugural[1:5])
    mt <- dfm_trim(mt, min_termfreq = 10)
    expect_warning(textplot_wordcloud(mt, min.freq = 10), "min.freq is deprecated")
    expect_warning(textplot_wordcloud(mt, use.r.layout = 10), "use.r.layout is no longer use")
    dev.off()
})



test_that("test textplot_keyness: show_reference works correctly ", {
    prescorpus <- corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump"))
    presdfm <- dfm(prescorpus, groups = "President", remove = stopwords("english"),
                   remove_punct = TRUE)
    result <- textstat_keyness(presdfm, target = "Trump")

    k <- 10
    p1 <- textplot_keyness(result, show_reference = FALSE, n = k)
    p2 <- textplot_keyness(result, show_reference = TRUE, n = k)

    # raises error when min_count is too high
    expect_error(textplot_keyness(result, show_reference = FALSE, min_count = 100),
                 "Too few words in the documents")
    # plot with two different fills when show_reference = TRUE
    expect_equal(dim(table(ggplot2::ggplot_build(p1)$data[[1]]$colour)), 1)
    expect_equal(dim(table(ggplot2::ggplot_build(p2)$data[[1]]$colour)), 2)

    # number of words plotted doubled when show_reference = TRUE
    expect_equal(nrow(ggplot2::ggplot_build(p1)$data[[1]]), k)
    expect_equal(nrow(ggplot2::ggplot_build(p2)$data[[1]]), 2 * k)

    # works with integer color
    expect_silent(textplot_keyness(result, color = 1:2))

    # test that textplot_keyness works with pallette (vector > 2 colors)
    expect_silent(textplot_keyness(result, show_reference = TRUE,
                                   color = RColorBrewer::brewer.pal(6, "Dark2")))

})

test_that("multiple patterns display correctly in textplot_kwic", {
    skip("For interactive visual inspection only")
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic_char_f <- kwic(toks, "f", window = 3)
    kwic_char_u <- kwic(toks, "u", window = 3)
    kwic_char_uf <- kwic(toks, c("u", "f"), window = 3)
    kwic_char_fu <- kwic(toks, c("f", "u"), window = 3)
    kwic_dict_u <- kwic(toks, dictionary(list(ukey = "u")), window = 3)
    kwic_dict_f <- kwic(toks, dictionary(list(fkey = "f")), window = 3)
    kwic_dict_uf <- kwic(toks, dictionary(list(ukey = "u", fkey = "f")), window = 3)
    kwic_dict_fu <- kwic(toks, dictionary(list(fkey = "f", ukey = "u")), window = 3)
    kwic_dict_uf_jm <- kwic(toks, dictionary(list(ufkey = c("u", "f"),
                                                  jmkey = c("j", "m"))), window = 3)

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf_jm, scale = "absolute")
})

test_that("phrasal patterns display correctly in textplot_kwic", {
    skip("For interactive visual inspection only")
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic_char_bc <- kwic(toks, phrase("b c"), window = 3)
    kwic_dict_bc <- kwic(toks, dictionary(list(bc = "b c")), window = 3)
    kwic_list_bc <- kwic(toks, list(bc = c("b", "c")), window = 3)

    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_bc, scale = "absolute")
    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_bc, scale = "absolute")
    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_list_bc, scale = "absolute")
})

dev.off()

test_that("plotting empty dfms after trimming is caught (#1755)", {
    dfmat <- dfm(c("Azymuth", "Compass", "GPS", "Zenith"))
    expect_error(
        textplot_wordcloud(dfmat, min_count = 2),
        "No features left after trimming with min_count = 2"
    )
})
