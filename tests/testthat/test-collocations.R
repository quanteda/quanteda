test_that("object2id is working with collocations", {
    txt <- c(". . . . a b c . . a b c . . . c d e",
             "a b . . a b . . a b . . a b . a b",
             "b c d . . b c . b c . . . b c")
    toks <- tokens(txt)
    type <- types(toks)
    load("../data/collocations/col.rda")
    # col <- textstat_collocations(toks, size = 2:3)
    ids <- quanteda:::object2id(col, type, 'fixed', TRUE)
    expect_equivalent(col$collocation,
                      vapply(ids, function(x, y) paste0(y[x], collapse = " "), character(1), type))
    expect_equal(names(ids), col$collocation)
})

test_that("tokens_compound works as expected with collocations", {
    toks <- tokens("The new law included capital gains taxes and inheritance taxes.")
    cols <- data.frame(collocation = c("the new", "capital gains", "gains taxes"), stringsAsFactors = FALSE)
    class(cols) <- c("collocations", "data.frame")

    expect_true(all(
      c("The_new", "capital_gains", "gains_taxes") %in%
      as.character(tokens_compound(toks, as.phrase(cols), join = FALSE))
    ))

    expect_true(all(
      c("The_new", "capital_gains_taxes") %in%
        as.character(tokens_compound(toks, as.phrase(cols), join = TRUE))
    ))

    expect_true(all(
      c("The_new", "capital_gains_taxes") %in%
        as.character(tokens_compound(toks, cols, case_insensitive = TRUE))
    ))

    expect_true(all(
      c("capital_gains_taxes") %in%
        as.character(tokens_compound(toks, cols, case_insensitive = FALSE))
    ))

    expect_equal(
         tokens_compound(toks, cols),
         tokens_compound(toks, as.phrase(cols))
     )
    expect_equal(
        tokens_compound(toks, cols, join = TRUE),
        tokens_compound(toks, as.phrase(cols), join = TRUE)
    )
})


# context("tokens_select feature selection works according to new scheme")

test_that("tokens_select works correctly with collocations objects", {
  txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
  toks_uni <- tokens(txt)
  toks_bi <- tokens(txt) |> tokens_ngrams(n = 2, concatenator = " ")
  # coll_bi <- textstat_collocations(toks_uni, size = 2, min_count = 2)
  # coll_tri <- textstat_collocations(toks_uni, size = 3, min_count = 2)[1, ]
  load("../data/collocations/coll_bi.rda")
  load("../data/collocations/coll_tri.rda")
  
  expect_equal(
        as.list(tokens_select(toks_uni, coll_bi$collocation)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_remove(toks_uni, phrase(coll_bi$collocation))),
        list(d1 = c("c", "d"), d2 = c("i", "j"))
    )
    expect_equal(
        as.list(tokens_remove(toks_uni, coll_bi)),
        as.list(tokens_remove(toks_uni, phrase(coll_bi$collocation)))
    )
    expect_equal(
        as.list(tokens_select(toks_uni, coll_tri$collocation)),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_select(toks_bi, coll_bi$collocation)),
        list(d1 = c("a b", "e g", "g h"), d2 = c("a b", "e g", "g h"))
    )
    expect_equal(
        as.list(tokens_select(toks_bi, phrase(coll_bi$collocation))),
        list(d1 = character(), d2 = character())
    )
    expect_equal(
        as.list(tokens_select(toks_bi, phrase(coll_bi$collocation))),
        as.list(tokens_select(toks_bi, coll_bi))
    )
    expect_equal(
        as.list(tokens_select(toks_bi, coll_tri)),
        list(d1 = character(), d2 = character())
    )
    expect_silent(
        tokens_select(toks_bi, coll_tri)
    )
})

test_that("tokens_select on unigrams works as expected when padding = TRUE", {
  txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
  toks_uni <- tokens(txt)
  # coll_bi <- textstat_collocations(toks_uni, size = 2, min_count = 2)
  # coll_tri <- textstat_collocations(toks_uni, size = 3, min_count = 2)[1, ]
  load("../data/collocations/coll_bi.rda")
  load("../data/collocations/coll_tri.rda")

  expect_equal(
    as.list(tokens_select(toks_uni, coll_bi$collocation, padding = TRUE)),
    list(d1 = rep("", 7), d2 = rep("", 7))
  )

  expect_equal(
    as.list(tokens_select(toks_uni, phrase(coll_bi$collocation), padding = TRUE)),
    list(d1 = c("a", "b", "", "", "e", "g", "h"),
         d2 = c("a", "b", "e", "g", "h", "", ""))
  )

  expect_equal(
    as.list(tokens_select(toks_uni, phrase(coll_bi$collocation), padding = TRUE)),
    as.list(tokens_select(toks_uni, coll_bi, padding = TRUE))
  )
})

test_that("tokens_select on bigrams works as expected when padding = TRUE", {
  txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
  toks_uni <- tokens(txt)
  toks_bi <- tokens(txt) |> tokens_ngrams(n = 2, concatenator = " ")
  # coll_bi <- textstat_collocations(toks_uni, size = 2, min_count = 2)
  # coll_tri <- textstat_collocations(toks_uni, size = 3, min_count = 2)[1, ]
  load("../data/collocations/coll_bi.rda")
  load("../data/collocations/coll_tri.rda")

    expect_equal(
        as.list(tokens_select(toks_bi, coll_bi$collocation, padding = TRUE)),
        list(d1 = c("a b", "", "", "", "e g", "g h"),
             d2 = c("a b", "", "e g", "g h", "", ""))
    )

    expect_equal(
        as.list(tokens_select(toks_bi, phrase(coll_bi$collocation), padding = TRUE)),
        list(d1 = rep("", 6), d2 = rep("", 6))
    )

    expect_equal(
        as.list(tokens_select(toks_bi, phrase(coll_bi$collocation), padding = TRUE)),
        as.list(tokens_select(toks_bi, coll_bi, padding = TRUE))
    )

    expect_silent(
        as.list(tokens_select(toks_bi, coll_bi, padding = TRUE))
    )
})

test_that("kwic works as expected with and without collocations phrases", {
    txt <- c(d1 = "a b c d e g h",  d2 = "a b e g h i j")
    toks_uni <- tokens(txt)
    toks_bi <- tokens(txt) |> tokens_ngrams(n = 2, concatenator = " ")

    dfm_uni <- dfm(toks_uni)
    dict_bi <- dictionary(list(one = "a b", two = "g j"))
    coll_bi <- data.frame(collocation = c("a b", "e g", "g h"),
                          stringsAsFactors = FALSE)
    class(coll_bi) <- c("collocations", "data.frame")
    coll_tri <- data.frame(collocation = c("e g h"),
                           stringsAsFactors = FALSE)
    class(coll_tri) <- c("collocations", "data.frame")

    
    expect_equal(
        as.data.frame(kwic(toks_uni, coll_bi))$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        as.data.frame(kwic(toks_uni, coll_tri))$keyword,
        c("e g h", "e g h")
    )

    expect_equal(
        as.data.frame(kwic(toks_uni, as.phrase(coll_bi)))$keyword,
        c("a b", "e g", "g h",
          "a b", "e g", "g h")
    )
    expect_equal(
        as.data.frame(kwic(toks_uni, as.phrase(dict_bi)))$keyword,
        c("a b", "a b")
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
})

test_that("char_select works with collocations pattern", {
    txt <- c(c1 = "aa bb", c2 = "ab", c3 = "aa bc", c4 = "bcd", c5 = "bcd")
    # patt <- textstat_collocations("aa bb aa bb aa bc bcd", min_count = 1)
    load("../data/collocations/patt.rda")

    expect_identical(
        char_keep(txt, patt),
        c(c1 = "aa bb", c3 = "aa bc")
    )
    expect_identical(
        char_remove(txt, patt),
        c(c2 = "ab", c4 = "bcd", c5 = "bcd")
    )
})

test_that("test phrase for collocations", {
    toks <- tokens(c("United States", "Congress", "federal government"))
    # colls <- textstat_collocations(toks, min_count = 1, tolower = FALSE)
    colls <- structure(list(collocation = c("United States", "federal government"), 
                            count = c(1L, 1L), 
                            count_nested = c(0L, 0L), 
                            length = c(2, 2), 
                            lambda = c(2.19722457733622, 2.19722457733622), 
                            z = c(0.951426150896346, 0.951426150896346)), 
                       row.names = c("1", "2"), class = c("collocations", "textstat", "data.frame"), 
                       types = c("United", "States", "Congress", "federal", "government"))
    expect_equivalent(
        as.phrase(colls),
        list(c("United", "States"), c("federal", "government"))
    )
})

test_that("is.collocations works", {
    load("../data/collocations/col.rda")
    expect_true(is.collocations(col))
    expect_false(is.collocations(10))
    expect_false(is.collocations(data_corpus_inaugural))
})
