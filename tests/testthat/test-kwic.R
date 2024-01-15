test_that("test kwic general", {
    toks <- tokens(paste(LETTERS, collapse = " "))
    expect_equal(
        as.data.frame(kwic(toks, "D")),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pre = "A B C",
            keyword = "D",
            post = "E F G H I",
            pattern = factor("D"),
            stringsAsFactors = FALSE)
    )
    
    expect_equal(
        as.data.frame(kwic(toks, "D", window = 2)),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pre = "B C",
            keyword = "D",
            post = "E F",
            pattern = factor("D"),
            stringsAsFactors = FALSE))
    
    expect_equal(
        as.data.frame(kwic(toks, "D", window = 2, separator = "_")),
        data.frame(
            docname = c("text1"),
            from = 4L,
            to = 4L,
            pre = "B_C",
            keyword = "D",
            post = "E_F",
            pattern = factor("D"),
            stringsAsFactors = FALSE))

    expect_equal(
        as.data.frame(kwic(toks, "D", separator = "")),
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
    testkwic <- kwic(tokens(paste(LETTERS, collapse = " ")), "A")
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
    testkwic <- kwic(tokens(paste(LETTERS, collapse = " ")), "Z")
    expect_equivalent(
        as.data.frame(testkwic),
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
    testkwic <- kwic(tokens(txt), c("D", "E"), window = 3)
    expect_equivalent(
        as.data.frame(testkwic),
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
    testkwic <- kwic(tokens(paste(LETTERS, collapse = " ")), "É")
    expect_true(is.data.frame(testkwic))
})

test_that("test kwic on multiple texts", {
    testcorpus <- corpus(c(
        paste(LETTERS[2:26], collapse = " "),
        paste(LETTERS, collapse = " ")
    ))
    testkwic <- kwic(tokens(testcorpus), "A")
    expect_that(
        as.data.frame(testkwic),
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
    testkwic <- kwic(tokens(testcorpus), "A")
    expect_that(
        as.data.frame(testkwic),
        equals(data.frame(
            docname = c(c("text1", "text1")),
            from = c(1L, 27L),
            to = c(1L, 27L),
            pre = c("", "V W X Y Z"),
            keyword = c("A", "A"),
            post = c("B C D E F", "B C D E F"),
            pattern = factor(c("A", "A")),
            stringsAsFactors = FALSE
        ))
    )
})

test_that("test kwic with multiple matches, where one is the last (fixed bug)", {
    testkwic <- kwic(tokens("what does the fox say fox"), "fox")
    expect_that(
        as.data.frame(testkwic),
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
    kwic_glob <- kwic(tokens(txt), "secur*", valuetype = "glob", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 as.data.frame(kwic_glob)$keyword)
    )

    kwic_glob2 <- kwic(tokens(txt), "secur*", valuetype = "glob", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 as.data.frame(kwic_glob2)$keyword)
    )
})

test_that("test that kwic works for regex types", {
    txt <- data_corpus_inaugural["2005-Bush"]
    kwic_regex <- kwic(tokens(txt), "^secur",valuetype = "regex", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 as.data.frame(kwic_regex)$keyword)
    )

    kwic_regex2 <- kwic(tokens(txt), "^secur", valuetype = "regex", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 as.data.frame(kwic_regex2)$keyword)
    )

})

test_that("test that kwic works for fixed types", {
    kwic_fixed <- kwic(tokens(data_corpus_inaugural), "security", valuetype = "fixed",
                       case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "Security"),
                 as.data.frame(kwic_fixed)$keyword)
    )

    kwic_fixed2 <- kwic(tokens(data_corpus_inaugural), "security", valuetype = "fixed",
                        case_insensitive = FALSE)
    expect_true(
        setequal(c("security"),
                 as.data.frame(kwic_fixed2)$keyword)
    )
})

test_that("test that kwic works with index", {
  toks <- tokens(data_corpus_inaugural)
  idx <- index(toks, "security")
  kiwc_idx <- kwic(toks, index = idx)
  kwic_pat <- kwic(toks, pattern = "security")
  expect_identical(kiwc_idx, kwic_pat)
  
  kiwc_idx2 <- kwic(toks, index = idx[c(2, 3, 1),])
  kwic_pat2 <- kwic(toks, pattern = "security")[c(2, 3, 1),]
  expect_identical(kiwc_idx2, kwic_pat2)
  
  kiwc_idx3 <- kwic(toks, index = idx[0,])
  kwic_pat3 <- kwic(toks, pattern = "security")[0,]
  expect_identical(kiwc_idx3, kwic_pat3)
  
  expect_error(kwic(toks),
               "Either pattten or index must be provided")
  expect_error(kwic(toks, index = data.frame(1:5)),
               "Invalid index object")
})

test_that("is.kwic works as expected", {
    kwic1 <- kwic(tokens(data_corpus_inaugural[1:3]), "provident*")
    expect_true(is.kwic(kwic1))
    expect_false(is.kwic("Not a kwic"))

    kwic2 <- kwic(tokens(data_corpus_inaugural[1:3]), "abcdefg")
    expect_true(is.kwic(kwic2))
})

test_that("print method works as expected", {
    testkwic <- kwic(tokens("what does the fox say fox"), "fox")
    expect_output(
        print(testkwic), 
        paste("Keyword-in-context with 2 matches.",
              "[text1, 4]         what does the | fox | say fox",
              "[text1, 6] what does the fox say | fox |", sep = "\\s*"
        ))
        
    testkwic <- kwic(tokens("what does the fox say fox"), "foox")
    expect_output(print(testkwic), "Keyword-in-context with 0 matches.", fixed = TRUE)
    
    toks <- tokens(data_corpus_inaugural[1:8])
    kw <- kwic(toks, "secure*", window = 1)
    out <- paste("Keyword-in-context with 6 matches.",
      "[1797-Adams, 478]   and | secure  | the",
      "[1797-Adams, 1512]   and | secured | immortal",
      "[1805-Jefferson, 2367] shall | secure  | to",
      "[1817-Monroe, 1754]    To | secure  | us",
      "[1817-Monroe, 1814]    to | secure  | our",
      "[1817-Monroe, 3009]    to | secure  | economy", sep = "\\s*")
    expect_output(print(kw, max_nrow = -1), out)
    expect_output(print(kw, max_nrow = 6), out)
    expect_output(print(kw, max_nrow = 7), out)
    expect_output(print(kw, show_summary = FALSE),
                  paste("[1797-Adams, 478]   and | secure  | the",   
                        "[1797-Adams, 1512]   and | secured | immortal",
                        "[1805-Jefferson, 2367] shall | secure  | to",
                        "[1817-Monroe, 1754]    To | secure  | us",
                        "[1817-Monroe, 1814]    to | secure  | our",   
                        "[1817-Monroe, 3009]    to | secure  | economy", sep = "\\s*"))
    expect_output(print(kw, 3),
                  paste("Keyword-in-context with 6 matches.",
                        "[1797-Adams, 478]   and | secure  | the",
                        "[1797-Adams, 1512]   and | secured | immortal",
                        "[1805-Jefferson, 2367] shall | secure  | to",
                        "[ reached max_nrow ... 3 more matches ]", sep = "\\s*"))
    expect_output(print(kwic(toks, "secured", window = 1)),
                  "Keyword-in-context with 1 match.                                            
 [1797-Adams, 1512] and | secured | immortal", fixed = TRUE)
    expect_output(print(kwic(toks, "XXX", window = 1)),
                  "Keyword-in-context with 0 matches.")
})

test_that("kwic works with padding", {
    testtoks <- tokens("what does the fox say cat")
    expect_output(
        print(kwic(tokens_remove(testtoks, c("what", "the"), padding = TRUE), "fox")),
        paste("Keyword-in-context with 1 match.",
              "[text1, 4]  does | fox | say cat", sep = "\\s*")
    )
    expect_output(
        print(kwic(tokens_remove(testtoks, "*", padding = TRUE), "fox")),
        "Keyword-in-context with 0 matches.",
    )
})

test_that("kwic works as expected with and without phrases", {
    txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
    toks_uni <- tokens(txt)
    dfm_uni <- dfm(toks_uni)
    toks_bi <- tokens(txt) |> tokens_ngrams(n = 2, concatenator = " ")
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
        as.data.frame(kwic(toks_uni, as.phrase(coll_bi)))$keyword,
        c("a b", "e g", "g h", "a b", "e g", "g h")
    )
    expect_equal(
        nrow(kwic(toks_bi, as.phrase(coll_bi))),
        0
    )

    expect_equal(nrow(kwic(toks_uni, dict_bi)), 2)
})

test_that("kwic error when dfm is given, #1006", {
    toks <- tokens("a b c")
    expect_error(kwic(toks, dfm(tokens("b c d"))))
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

    kwic_dict1 <- kwic(tokens(corp), dictionary(list(ukey = "u")))
    kwic_dict2 <- kwic(toks, dictionary(list(ukey = "u")))
    kwic_dict3 <- kwic(tokens(corp), dictionary(list(ukey = "u", fkey = "f")))
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
    kw <- c(d2 = "one two three four", d1 = "four three two one") |>
        tokens() |>
        kwic(pattern = c("two", "two", "three"))
    expect_equal(
        as.character(kw$pattern),
        char_tolower(as.data.frame(kw)$keyword)
    )
})

test_that("subsetting of kwic works", {
    kw <- kwic(tokens(data_corpus_inaugural), "terror")
    kw2 <- kw[1:3, ]
    expect_true("kwic" %in% class(kw2))
    expect_output(
        print(kw2),
        paste0("^Keyword-in-context with 3 matches\\.")
    )
})

test_that("pre and post for phrases are working", {
    toks <- tokens(c(doc1 = "a a a b c d d d", doc2 = "a b c d e"))
    expect_identical(
        as.data.frame(kwic(toks, phrase("b c"), window = 2)),
        structure(list(docname = c("doc1", "doc2"), 
                       from = c(4L, 2L), to = c(5L, 3L), 
                       pre = c("a a", "a"), keyword = c("b c", "b c"), 
                       post = c("d d", "d e"), 
                       pattern = structure(c(1L, 1L), .Label = "b c", class = "factor")), 
                  class = "data.frame", row.names = c(NA, -2L))
    )
})

test_that("kwic structure is as expected", {
    toks <- tokens(c(doc1 = "a a a b c d d d", 
                     doc2 = "a b c d e", 
                     doc3 = "b b a a"))
    kw <- kwic(toks, phrase("a a"), window = 2)
    expect_identical(
      kw,
      structure(data.frame(docname = c("doc1", "doc1", "doc3"), 
                           from = 1L:3L, to = 2L:4L, 
                           pre = c("", "a", "b b"), 
                           keyword = c("a a", "a a", "a a"), 
                           post = c("a b", "b c", ""), 
                           pattern = factor(rep("a a", 3)),
                           stringsAsFactors = FALSE),
                class = c("kwic", "data.frame"), 
                ntoken = c(doc1 = 8L, doc3 = 4L))
    )
})

test_that("kwic deprecations work as expected", {
    txt <- "A b c d e."
    expect_warning(
      kwic(tokens(txt), "e", window = 2, remove_punct = TRUE),
      "remove_punct argument is not used"
    )
})
