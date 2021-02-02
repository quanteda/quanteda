context("test kwic")

# test_that("test attr(kwic, 'ntoken') with un-named texts", {
#     txt <- c(
#         "The quick brown fox jumped over the lazy dog",
#         "The quick brown fox",
#         "The quick brown dog jumped over the lazy dog",
#         NA
#     )
#     kw <- suppressWarnings(kwic(txt, "fox"))
# 
#     expect_equal(
#         attr(kw, "ntoken"),
#         c("text1" = 9, "text2" = 4, "text3" = 9, "text4" = 0)
#     )
# })
# 
# test_that("test attr(kwic, 'ntoken') text names", {
#     kw <- kwic(data_corpus_inaugural, "american")
#     expect_equal(
#         names(attr(kw, "ntoken")),
#         names(texts(data_corpus_inaugural))
#     )
# })

test_that("test kwic general", {
    txt <- paste(LETTERS, collapse = " ")
    expect_equal(
        as.data.frame(kwic(txt, "D")),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pattern = factor("D"),
            pre = "A B C",
            keyword = "D",
            post = "E F G H I",
            stringsAsFactors = FALSE))
    
    expect_equal(
        as.data.frame(kwic(txt, "D"), window = 2),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pattern = factor("D"),
            pre = "B C",
            keyword = "D",
            post = "E F",
            stringsAsFactors = FALSE))
    
    expect_equal(
        as.data.frame(kwic(txt, "D"), window = 2, separator = "_"),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pattern = factor("D"),
            pre = "B_C",
            keyword = "D",
            post = "E_F",
            stringsAsFactors = FALSE))

    expect_equal(
        as.data.frame(kwic(txt, "D"), separator = ""),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pattern = factor("D"),
            pre = "ABC",
            keyword = "D",
            post = "EFGHI",
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
            pattern = factor("A"),
            pre = "",
            keyword = "A",
            post = "B C D E F",
            stringsAsFactors = FALSE
        )
    )
})

test_that("test kwic on last token", {
    testkwic <- kwic(paste(LETTERS, collapse = " "), "Z")
    expect_equivalent(
        as.data.frame(testkwic),
        data.frame(
            docname = c("text1"),
            from = 26L,
            to = 26L,
            pattern = factor("Z"),
            pre = "U V W X Y",
            keyword = "Z",
            post = "",
            stringsAsFactors = FALSE
        )
    )
})

test_that("test kwic on two tokens", {
    txt <- "A B C D E F G D H"
    testkwic <- kwic(txt, c("D", "E"))
    expect_equivalent(
        as.data.frame(testkwic, window = 3),
        data.frame(
            docname = "text1",
            from = c(4L, 5L, 8L),
            to = c(4L, 5L, 8L),
            pattern = factor(c("D", "E", "D")),
            pre = c("A B C", "B C D", "E F G"),
            keyword = c("D", "E", "D"),
            post = c("E F G", "F G D", "H"),
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
        as.data.frame(testkwic),
        equals(data.frame(
            docname = c("text2"),
            from = 1L,
            to = 1L,
            pattern = factor("A"),
            pre = "",
            keyword = "A",
            post = "B C D E F",
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
        as.data.frame(testkwic),
        equals(data.frame(
            docname = c(c("text1", "text1")),
            from = c(1L, 27L),
            to = c(1L, 27L),
            pattern = factor(c("A", "A")),
            pre = c("", "V W X Y Z"),
            keyword = c("A", "A"),
            post = c("B C D E F", "B C D E F"),
            stringsAsFactors = FALSE
        ))
    )
})

test_that("test kwic with multiple matches, where one is the last (fixed bug)", {
    testkwic <- kwic("what does the fox say fox", "fox")
    expect_that(
        as.data.frame(testkwic),
        equals(data.frame(
            docname = c(c("text1", "text1")),
            from = c(4L, 6L),
            to = c(4L, 6L),
            pattern = factor("fox"),
            pre = c("what does the", "what does the fox say"),
            keyword = c("fox", "fox"),
            post = c("say fox", ""),
            stringsAsFactors = F
        ))
    )
})

test_that("test that kwic works for glob types", {
    txt <- data_corpus_inaugural["2005-Bush"]
    kwic_glob <- kwic(txt, "secur*", valuetype = "glob", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 as.data.frame(kwic_glob)$keyword)
    )

    kwic_glob2 <- kwic(txt, "secur*", valuetype = "glob", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 as.data.frame(kwic_glob2)$keyword)
    )

})

test_that("test that kwic works for regex types", {

    txt <- data_corpus_inaugural["2005-Bush"]
    kwic_regex <- kwic(txt, "^secur",valuetype = "regex", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 as.data.frame(kwic_regex)$keyword)
    )

    kwic_regex2 <- kwic(txt, "^secur", valuetype = "regex", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 as.data.frame(kwic_regex2)$keyword)
    )

})

test_that("test that kwic works for fixed types", {
    kwic_fixed <- kwic(data_corpus_inaugural, "security", valuetype = "fixed",
                       case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "Security"),
                 as.data.frame(kwic_fixed)$keyword)
    )

    kwic_fixed2 <- kwic(data_corpus_inaugural, "security", valuetype = "fixed",
                        case_insensitive = FALSE)
    expect_true(
        setequal(c("security"),
                 as.data.frame(kwic_fixed2)$keyword)
    )
})

test_that("is.kwic works as expected", {
    kwic1 <- kwic(data_corpus_inaugural[1:3], "provident*")
    expect_true(is.kwic(kwic1))
    expect_false(is.kwic("Not a kwic"))

    kwic2 <- kwic(data_corpus_inaugural[1:3], "abcdefg")
    expect_true(is.kwic(kwic2))
})

test_that("print method works as expected", {
    testkwic <- kwic("what does the fox say fox", "fox")
    expect_output(
        print(testkwic), 
        paste0("Keyword-in-context with 2 matches\\.\\s*",                                      
               " [text1, 4]         what does the | fox | say fox\\s*",
               " [text1, 6] what does the fox say | fox |\\s*"),
        )
        
    testkwic <- kwic("what does the fox say fox", "foox")
    expect_output(print(testkwic), "Keyword-in-context with 0 matches")
})

test_that("kwic works with padding", {
    testtoks <- tokens("what does the fox say cat")
    expect_output(
        print(kwic(tokens_remove(testtoks, c("what", "the"), padding = TRUE), "fox")),
        paste0("Keyword-in-context with 1 matches\\.\\s*", 
               " [text1, 4] does | fox | say cat\\s*"))
    expect_output(
        print(kwic(tokens_remove(testtoks, "*", padding = TRUE), "fox")),
        "Keyword-in-context with 0 matches"
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
        as.data.frame(kwic(txt, char_uni))$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        as.data.frame(kwic(txt, list_uni))$keyword,
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
        as.data.frame(kwic(txt, list(c("c", "d"), c("g", "h"))))$keyword,
        c("c d", "g h", "g h")
    )
    expect_equal(
        as.data.frame(kwic(txt, phrase(c("c d", "g h"))))$keyword,
        c("c d", "g h", "g h")
    )

    expect_equal(
        as.data.frame(kwic(txt, coll_bi))$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        as.data.frame(kwic(txt, coll_tri))$keyword,
        c("e g h", "e g h")
    )

    expect_equal(
        as.data.frame(kwic(txt, phrase(coll_bi)))$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        as.data.frame(kwic(txt, phrase(dict_bi)))$keyword,
        c("a b", "a b")
    )
    expect_equal(
        as.data.frame(kwic(txt, dict_bi))$keyword,
        c("a b", "a b")
    )

    ## on tokens
    expect_equal(
        as.data.frame(kwic(toks_uni, char_uni))$keyword,
        c("a", "b", "g",
          "a", "b", "g", "j")
    )
    expect_equal(
        as.data.frame(kwic(toks_uni, list_uni))$keyword,
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
        as.data.frame(kwic(toks_uni, list(c("c", "d"), c("g", "h"))))$keyword,
        c("c d", "g h", "g h")
    )
    expect_equal(
        as.data.frame(kwic(toks_uni, phrase(c("c d", "g h"))))$keyword,
        c("c d", "g h", "g h")
    )

    expect_equal(nrow(kwic(toks_uni, coll_bi)), 6)
    expect_equal(nrow(kwic(toks_uni, coll_tri)), 2)

    expect_equal(
        as.data.frame(kwic(toks_uni, phrase(coll_bi)))$keyword,
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
    kwic1 <- kwic(toks, "f")
    kwic2 <- kwic(toks, "u")
    kwic3 <- kwic(toks, c("u", "f"))

    expect_identical(kwic1$pattern, factor(c("f", "f")))
    expect_identical(kwic2$pattern, factor(c("u", "u")))
    expect_identical(kwic3$pattern, factor(c("f", "u", "f", "u"), levels = c("u", "f")))

    kwic_dict1 <- kwic(corp, dictionary(list(ukey = "u")))
    kwic_dict2 <- kwic(toks, dictionary(list(ukey = "u")))
    kwic_dict3 <- kwic(corp, dictionary(list(ukey = "u", fkey = "f")))
    kwic_dict4 <- kwic(toks, dictionary(list(ukey = "u", fkey = "f")))

    expect_identical(kwic_dict1, kwic_dict2)
    expect_identical(kwic_dict3, kwic_dict4)
    expect_identical(kwic_dict1$pattern, factor(c("ukey", "ukey")))
    expect_identical(kwic_dict3$pattern, factor(rep(c("fkey", "ukey"), 2),
                                                levels = c("ukey", "fkey")))

    col <- data.frame(collocations = c("u v", "e f"), stringsAsFactors = FALSE)
    class(col) <- c("collocations", "data.frame")
    kwic_col <- kwic(toks, col)
    expect_identical(kwic_col$pattern, factor(c("e f", "u v", "e f", "u v"),
                                              levels = c("u v", "e f")))
})

test_that("keywords match pattern match and map_keywords() is working as expected", {
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic1 <- kwic(toks, dictionary(list(key1 = c("a", "b"), key2 = c("x", "y"))))
    expect_equal(
        kwic1$pattern,
        factor(c("key1", "key1", "key2", "key2", "key1", "key1", "key2", "key2"),
               levels = c("key1", "key2"))
    )

    kwic2 <- kwic(toks, dictionary(list(key2 = c("x", "y"), key1 = c("a", "b"))))
    expect_equal(
        kwic2$pattern,
        factor(c("key1", "key1", "key2", "key2", "key1", "key1", "key2", "key2"),
               levels = c("key2", "key1"))
    )

    kwic3 <- kwic(toks, dictionary(list(key2 = c("b", "c"), key1 = c("a", "b"))))
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
        char_tolower(as.data.frame(kw1)$keyword)
    )

    kw2 <- kwic(toks, phrase("is a"), valuetype = "fixed")
    expect_equal(
        as.character(kw2$pattern),
        char_tolower(as.data.frame(kw2)$keyword)
    )
})

test_that("kwic with pattern overlaps works as expected", {
    kw <- c(d2 = "one two three four", d1 = "four three two one") %>%
        tokens() %>%
        kwic(pattern = c("two", "two", "three"))
    expect_equal(
        as.character(kw$pattern),
        char_tolower(as.data.frame(kw)$keyword)
    )
})

test_that("subsetting of kwic works", {
    kw <- kwic(data_corpus_inaugural, "terror")
    kw2 <- kw[1:3, ]
    expect_true("kwic" %in% class(kw2))
    expect_output(
        print(kw2),
        paste0("Keyword-in-context with 3 matches\\.*")
    )
})

test_that("raise error when inputs are invalid", {
    
    toks <-tokens( "This is a sample text.")
    expect_error(
        print(kwic(toks, "sample"), window = c(1, 1, 3)),
        "The length of window must be 1"
    )
    expect_error(
        print(kwic(toks, "sample"), window = -1),
        "The value of window must be between 0 and Inf"
    )
    
    expect_error(
        print(kwic(toks, "sample"), separator = character()),
        "The length of separator must be 1"
    )
})
