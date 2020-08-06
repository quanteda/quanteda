context("test kwic")

test_that("test attr(kwic, 'ntoken') with un-named texts", {
    txt <- c(
        "The quick brown fox jumped over the lazy dog",
        "The quick brown fox",
        "The quick brown dog jumped over the lazy dog",
        NA
    )
    kw <- kwic(txt, "fox")

    expect_equal(
        attr(kw, "ntoken"),
        c("text1" = 9, "text2" = 4, "text3" = 9, "text4" = 0)
    )
})

test_that("test attr(kwic, 'ntoken') text names", {
    kw <- kwic(data_corpus_inaugural, "american")
    expect_equal(
        names(attr(kw, "ntoken")),
        names(texts(data_corpus_inaugural))
    )
})

test_that("test kwic general", {
    txt <- paste(LETTERS, collapse = " ")
    expect_equal(
        data.frame(kwic(txt, "D")),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pre = "A B C",
            keyword = "D",
            post = "E F G H I",
            pattern = factor("D"),
            stringsAsFactors = FALSE))

    expect_equal(
        data.frame(kwic(txt, "D", separator = "")),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pre = "ABC",
            keyword = "D",
            post = "EFGHI",
            pattern = factor("D"),
            stringsAsFactors = FALSE))
})


test_that("test kwic on first token", {
    testkwic <- kwic(paste(LETTERS, collapse = " "), "A")
    expect_equivalent(
        as.data.frame(testkwic),
        data.frame(
            docname = "text1",
            from = 1L,
            to = 1L,
            pre = "",
            keyword = "A",
            post = "B C D E F",
            pattern = factor("A"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("test kwic on last token", {
    testkwic <- kwic(paste(LETTERS, collapse = " "), "Z")
    expect_equivalent(
        data.frame(testkwic),
        data.frame(
            docname = c("text1"),
            from = 26L,
            to = 26L,
            pre = "U V W X Y",
            keyword = "Z",
            post = "",
            pattern = factor("Z"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("test kwic on two tokens", {
    txt <- "A B C D E F G D H"
    testkwic <- kwic(txt, c("D", "E"), 3)
    expect_equivalent(
        testkwic,
        data.frame(
            docname = "text1",
            from = c(4L, 5L, 8L),
            to = c(4L, 5L, 8L),
            pre = c("A B C", "B C D", "E F G"),
            keyword = c("D", "E", "D"),
            post = c("E F G", "F G D", "H"),
            pattern = factor(c("D", "E", "D")),
            stringsAsFactors = FALSE)
    )
})

test_that("test kwic on non-existent token", {
    testkwic <- kwic(paste(LETTERS, collapse = " "), "É")
    expect_true(is.data.frame(testkwic))
})

test_that("test kwic on multiple texts", {
    testcorpus <- corpus(c(
        paste(LETTERS[2:26], collapse = " "),
        paste(LETTERS, collapse = " ")
    ))
    testkwic <- kwic(testcorpus, "A")
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c("text2"),
            from = 1L,
            to = 1L,
            pre = "",
            keyword = "A",
            post = "B C D E F",
            pattern = factor("A"),
            stringsAsFactors = FALSE
        ))
    )
})

test_that("test kwic with multiple matches", {
    testcorpus <- corpus(c(
        paste(c(LETTERS, LETTERS), collapse = " ")
    ))
    testkwic <- kwic(testcorpus, "A")
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c(c("text1", "text1")),
            from = c(1L, 27L),
            to = c(1L, 27L),
            pre = c("", "V W X Y Z"),
            keyword = c("A", "A"),
            post = c("B C D E F", "B C D E F"),
            pattern = factor(c("A", "A")),
            stringsAsFactors = F
        ))
    )
})

test_that("test kwic with multiple matches, where one is the last (fixed bug)", {
    testkwic <- kwic("what does the fox say fox", "fox")
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c(c("text1", "text1")),
            from = c(4L, 6L),
            to = c(4L, 6L),
            pre = c("what does the", "what does the fox say"),
            keyword = c("fox", "fox"),
            post = c("say fox", ""),
            pattern = factor("fox"),
            stringsAsFactors = F
        ))
    )
})

test_that("test that kwic works for glob types", {
    txt <- data_corpus_inaugural["2005-Bush"]
    kwic_glob <- kwic(txt, "secur*", window = 3, valuetype = "glob", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 unique(kwic_glob$keyword))
    )

    kwic_glob2 <- kwic(txt, "secur*", window = 3, valuetype = "glob", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 unique(kwic_glob2$keyword))
    )

})

test_that("test that kwic works for regex types", {

    txt <- data_corpus_inaugural["2005-Bush"]
    kwic_regex <- kwic(txt, "^secur", window = 3, valuetype = "regex", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 unique(kwic_regex$keyword))
    )

    kwic_regex2 <- kwic(txt, "^secur", window = 3, valuetype = "regex", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 unique(kwic_regex2$keyword))
    )

})

test_that("test that kwic works for fixed types", {
    kwic_fixed <- kwic(data_corpus_inaugural, "security", window = 3, valuetype = "fixed",
                       case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "Security"),
                 unique(kwic_fixed$keyword))
    )

    kwic_fixed2 <- kwic(data_corpus_inaugural, "security", window = 3, valuetype = "fixed",
                        case_insensitive = FALSE)
    expect_true(
        setequal(c("security"),
                 unique(kwic_fixed2$keyword))
    )
})

test_that("is.kwic works as expected", {
    kwic1 <- kwic(data_corpus_inaugural[1:3], "provident*")
    expect_true(is.kwic(kwic1))
    expect_false(is.kwic("Not a kwic"))
    expect_false(is.kwic(kwic1[, c("pre", "post")]))

    kwic2 <- kwic(data_corpus_inaugural[1:3], "abcdefg")
    expect_true(is.kwic(kwic2))
})

test_that("textplot_xray works with new kwic, one token phrase", {
    data_corpus_inauguralpost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralpost70, "american")
    expect_silent(textplot_xray(knew))
})

test_that("textplot_xray works with new kwic, two token phrase", {
    data_corpus_inauguralpost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralpost70, phrase("american people"))
    expect_silent(textplot_xray(knew))
})

test_that("print method works as expected", {
    testkwic <- kwic("what does the fox say fox", "fox")
    expect_output(print(testkwic), "*\\| fox \\|*")
    expect_output(print(testkwic), "\\[text1, 4\\]*")

    testkwic <- kwic("what does the fox say fox", "foox")
    expect_output(print(testkwic), "kwic object with 0 rows")
})


test_that("kwic works with padding", {
    testtoks <- tokens("what does the fox say cat")
    expect_output(print(kwic(tokens_remove(testtoks, c("what", "the"), padding = TRUE), "fox")),
                  "\\[text1, 4\\]  does \\| fox \\| say cat")
    expect_output(
        print(kwic(tokens_remove(testtoks, "*", padding = TRUE), "fox")),
        "kwic object with 0 rows"
    )
})

test_that("kwic works as expected with and without phrases", {

    txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
    toks_uni <- tokens(txt)
    dfm_uni <- dfm(toks_uni)
    toks_bi <- tokens(txt) %>% tokens_ngrams(n = 2, concatenator = " ")
    dfm_bi <- dfm(toks_bi)
    char_uni <- c("a", "b", "g", "j")
    char_bi <- c("a b", "g j")
    list_uni <- list("a", "b", "g", "j")
    list_bi <- list("a b", "g j")
    dict_uni <- dictionary(list(one = c("a", "b"), two = c("g", "j")))
    dict_bi <- dictionary(list(one = "a b", two = "g j"))
    coll_bi <- data.frame(collocation = c("a b", "e g", "g h"),
                          stringsAsFactors = FALSE)
    class(coll_bi) <- c("collocations", "data.frame")
    coll_tri <- data.frame(collocation = c("e g h"),
                           stringsAsFactors = FALSE)
    class(coll_tri) <- c("collocations", "data.frame")

    expect_equal(
        kwic(txt, char_uni)$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        kwic(txt, list_uni)$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        nrow(kwic(txt, char_bi)),
        0
    )
    expect_equal(
        nrow(kwic(txt, list("c d", "g h"))),
        0
    )
    expect_equal(
        kwic(txt, list(c("c", "d"), c("g", "h")))$keyword,
        c("c d", "g h", "g h")
    )
    expect_equal(
        kwic(txt, phrase(c("c d", "g h")))$keyword,
        c("c d", "g h", "g h")
    )

    expect_equal(
        kwic(txt, coll_bi)$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        kwic(txt, coll_tri)$keyword,
        c("e g h", "e g h")
    )

    expect_equal(
        kwic(txt, phrase(coll_bi))$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        kwic(txt, phrase(dict_bi))$keyword,
        c("a b", "a b")
    )
    expect_equal(
        kwic(txt, dict_bi)$keyword,
        c("a b", "a b")
    )

    ## on tokens
    expect_equal(
        kwic(toks_uni, char_uni)$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        kwic(toks_uni, list_uni)$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        nrow(kwic(toks_uni, char_bi)),
        0
    )
    expect_equal(
        nrow(kwic(toks_uni, list("c d", "g h"))),
        0
    )
    expect_equal(
        kwic(toks_uni, list(c("c", "d"), c("g", "h")))$keyword,
        c("c d", "g h", "g h")
    )
    expect_equal(
        kwic(toks_uni, phrase(c("c d", "g h")))$keyword,
        c("c d", "g h", "g h")
    )

    expect_equal(nrow(kwic(toks_uni, coll_bi)), 6)
    expect_equal(nrow(kwic(toks_uni, coll_tri)), 2)

    expect_equal(
        kwic(toks_uni, phrase(coll_bi))$keyword,
        c("a b", "e g", "g h", "a b", "e g", "g h")
    )
    expect_equal(
        nrow(kwic(toks_bi, phrase(coll_bi))),
        0
    )

    expect_equal(nrow(kwic(toks_uni, dict_bi)), 2)
})


test_that("kwic error when dfm is given, #1006", {
    toks <- tokens("a b c")
    expect_error(kwic(toks, dfm("b c d")))
})

test_that("keywords attribute is set correctly in textplot_kwic (#1514)", {
    corp <- corpus(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))
    toks <- tokens(corp)
    kwic1 <- kwic(toks, "f", window = 3)
    kwic2 <- kwic(toks, "u", window = 3)
    kwic3 <- kwic(toks, c("u", "f"), window = 3)

    expect_identical(kwic1$pattern, factor(c("f", "f")))
    expect_identical(kwic2$pattern, factor(c("u", "u")))
    expect_identical(kwic3$pattern, factor(c("f", "u", "f", "u"), levels = c("u", "f")))

    kwic_dict1 <- kwic(corp, dictionary(list(ukey = "u")), window = 3)
    kwic_dict2 <- kwic(toks, dictionary(list(ukey = "u")), window = 3)
    kwic_dict3 <- kwic(corp, dictionary(list(ukey = "u", fkey = "f")), window = 3)
    kwic_dict4 <- kwic(toks, dictionary(list(ukey = "u", fkey = "f")), window = 3)

    expect_identical(kwic_dict1, kwic_dict2)
    expect_identical(kwic_dict3, kwic_dict4)
    expect_identical(kwic_dict1$pattern, factor(c("ukey", "ukey")))
    expect_identical(kwic_dict3$pattern, factor(rep(c("fkey", "ukey"), 2),
                                                levels = c("ukey", "fkey")))

    col <- data.frame(collocations = c("u v", "e f"), stringsAsFactors = FALSE)
    class(col) <- c("collocations", "data.frame")
    kwic_col <- kwic(toks, col, window = 3)
    expect_identical(kwic_col$pattern, factor(c("e f", "u v", "e f", "u v"),
                                              levels = c("u v", "e f")))
})

test_that("keywords match pattern match and map_keywords() is working as expected", {
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic1 <- kwic(toks, dictionary(list(key1 = c("a", "b"), key2 = c("x", "y"))), window = 3)
    expect_equal(
        kwic1$pattern,
        factor(c("key1", "key1", "key2", "key2", "key1", "key1", "key2", "key2"),
               levels = c("key1", "key2"))
    )

    kwic2 <- kwic(toks, dictionary(list(key2 = c("x", "y"), key1 = c("a", "b"))), window = 3)
    expect_equal(
        kwic2$pattern,
        factor(c("key1", "key1", "key2", "key2", "key1", "key1", "key2", "key2"),
               levels = c("key2", "key1"))
    )

    kwic3 <- kwic(toks, dictionary(list(key2 = c("b", "c"), key1 = c("a", "b"))), window = 3)
    expect_equal(
        kwic3$pattern,
        factor(c("key1", "key2", "key1", "key2", "key1", "key2", "key1", "key2"),
               levels =  c("key2", "key1"))
    )
})

test_that("kwic pattern column works for phrases", {
    txt <- c("This is a test",
          "This is it.",
          "What is in a train?",
          "Is it a question?",
          "Sometimes you don't know if this is it.",
          "Is it a bird or a plane or is it a train?")
    toks <- tokens(txt)

    kw1 <- kwic(toks, c("is", "a"), valuetype = "fixed")
    expect_equal(
        as.character(kw1$pattern),
        char_tolower(kw1$keyword)
    )

    kw2 <- kwic(toks, phrase("is a"), valuetype = "fixed")
    expect_equal(
        as.character(kw2$pattern),
        char_tolower(kw2$keyword)
    )
})

test_that("kwic with pattern overlaps works as expected", {
    kw <- c(d2 = "one two three four", d1 = "four three two one") %>%
        tokens() %>%
        kwic(pattern = c("two", "two", "three"), window = 1)
    expect_equal(
        as.character(kw$pattern),
        char_tolower(kw$keyword)
    )
})

test_that("subsetting and printing a subsetted kwic works (#1665)", {
    kw <- kwic(data_corpus_inaugural, "terror")
    expect_output(print(kw[, c("pre", "keyword", "post")]), "pre keyword")
    expect_true("kwic" %in% class(kw[1:3, ]))
    expect_false("kwic" %in% class(kw[1:3, 1:2]))
    expect_false("kwic" %in% class(kw[, 1:2]))
})
