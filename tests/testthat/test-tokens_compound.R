test_that("tokens_compound join tokens correctly", {

    txt <- c("a b c d e f g", "A B C D E F G", "A b C d E f G",
             "aaa bbb ccc ddd eee fff ggg", "a_b b_c c_d d_e e_f f_g")
    toks <- tokens(txt)
    seqs <- as.list(tokens(c("a b", "C D", "aa* bb*", "eEE FFf", "d_e e_f"),
                    what = "fastestword"))
    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = TRUE)),
        list(c("a_b", "c_d", "e", "f", "g"),
             c("A_B", "C_D", "E", "F", "G"),
             c("A_b", "C_d", "E", "f", "G"),
             c("aaa_bbb", "ccc", "ddd", "eee_fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )

    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = FALSE)),
        list(c("a_b", "c", "d", "e", "f", "g"),
             c("A", "B", "C_D", "E", "F", "G"),
             c("A", "b", "C", "d", "E", "f", "G"),
             c("aaa_bbb", "ccc", "ddd", "eee", "fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )

    seqs_fixed <- as.list(tokens(c("a b", "C D", "aa bb", "eEE FFf", "d_e e_f"),
                          what = "fastestword"))
    expect_equivalent(
        as.list(tokens_compound(toks, seqs_fixed, valuetype = "glob", case_insensitive = TRUE)),
        list(c("a_b", "c_d", "e", "f", "g"),
             c("A_B", "C_D", "E", "F", "G"),
             c("A_b", "C_d", "E", "f", "G"),
             c("aaa", "bbb", "ccc", "ddd", "eee_fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )

    expect_equivalent(
        as.list(tokens_compound(toks, seqs_fixed, valuetype = "glob", case_insensitive = FALSE)),
        list(c("a_b", "c", "d", "e", "f", "g"),
             c("A", "B", "C_D", "E", "F", "G"),
             c("A", "b", "C", "d", "E", "f", "G"),
             c("aaa", "bbb", "ccc", "ddd", "eee", "fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )
    
    expect_message(
        tokens_compound(toks, seqs, verbose = TRUE),
        "tokens_compound() changed", fixed = TRUE
    )
  
})

test_that("tokens_compound join a sequences of sequences", {

    txt <- c("a b c d e f g", "A B C D E F G")
    toks <- tokens(txt)
    seqs <- as.list(tokens(c("a b", "b c d", "E F", "F G"),
                    what = "fastestword"))
    expect_equal(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = TRUE, join = TRUE)),
        list(text1 = c("a_b_c_d", "e_f_g"),
             text2 = c("A_B_C_D", "E_F_G"))
    )

    expect_equal(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = TRUE, join = FALSE)),
        list(text1 = c("a_b", "b_c_d", "e_f", "f_g"),
             text2 = c("A_B", "B_C_D", "E_F", "F_G"))
    )
    
    expect_error(
        tokens_compound(toks, seqs, join = c(TRUE, FALSE)),
        "The length of join must be 1"
    )
})

test_that("keep_unigrams works", {
    
    txt <- "we like high quality sound"
    toks <- tokens(txt)
    
    # overlapped
    pat <- phrase(c("high quality", "quality sound"))
    expect_equal(as.list(tokens_compound(toks, pat, join = TRUE)),
                 list(text1 = c("we", "like", "high_quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat, join = FALSE)),
                 list(text1 = c("we", "like", "high_quality", "quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat, join = TRUE, keep_unigrams = TRUE)),
                 list(text1 = c("we", "like", "high", "quality", "sound",
                                "high_quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat, join = FALSE, keep_unigrams = TRUE)),
                 list(text1 = c("we", "like", "high", "quality", "high_quality",
                                "sound", "quality_sound")))
    
    # nested
    pat2 <- phrase(c("high quality", "quality sound", "high quality sound"))
    expect_equal(as.list(tokens_compound(toks, pat2, join = TRUE)),
                 list(text1 = c("we", "like", "high_quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat2, join = FALSE)),
                 list(text1 = c("we", "like", "high_quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat2, join = TRUE, keep_unigrams = TRUE)),
                 list(text1 = c("we", "like", "high", "quality", "sound",
                                "high_quality_sound")))
    expect_equal(as.list(tokens_compound(toks, pat2, join = FALSE, keep_unigrams = TRUE)),
                 list(text1 = c("we", "like", "high", "quality", "sound",
                                "high_quality_sound")))
    
    expect_error(
        tokens_compound(toks, seqs, keep_unigrams = c(TRUE, FALSE)),
        "The length of keep_unigrams must be 1"
    )

})

test_that("tokens_compound is not affected by the order of compounds", {
    expect_equal(
        as.list(tokens_compound(tokens("The people of the United States of America."),
                                c("United States of America", "United States"))),
        as.list(tokens_compound(tokens("The people of the United States of America."),
                                c("United States", "United States of America")))
    )
})

test_that("tokens_compound preserved document names", {
    expect_equal(
        names(tokens_compound(tokens(c(d1 = "The people of the United States of America.",
                                       d2 = "The United States is south of Canada.")),
                              c("United States", "United States of America"))),
        c("d1", "d2")
    )
})

test_that("tokens_compound works with padded tokens", {
    toks <- tokens(c(doc1 = "a b c d e f g"))
    toks <- tokens_remove(toks, c("b", "e"), padding = TRUE)
    toks <- tokens_compound(toks, phrase("c d"))
    expect_equal(sort(attr(toks, "types")),
                 sort(c("a", "c_d", "f", "g")))
})

test_that("tokens_compound works with different concatenators", {
  toks <- tokens(c(doc1 = "a b c d e f g"), concatenator = " ")
  
  toks1 <- tokens_compound(toks, phrase("c d"), concatenator = "+")
  expect_equal(meta(toks1, field = "concatenator", type = "object"), " ")
  expect_equal(sort(attr(toks1, "types")),
               sort(c("a", "b", "c+d", "e", "f", "g")))
  
  toks2 <- tokens_compound(toks, phrase("c d"), concatenator = "&&")
  expect_equal(meta(toks2, field = "concatenator", type = "object"), " ")
  expect_equal(sort(attr(toks2, "types")),
               sort(c("a", "b", "c&&d", "e", "f", "g")))
  
  toks3 <- tokens_compound(toks, phrase("c d"), concatenator = "")
  expect_equal(meta(toks3, field = "concatenator", type = "object"), " ")
  expect_equal(sort(attr(toks3, "types")),
               sort(c("a", "b", "cd", "e", "f", "g")))
  expect_error(tokens_compound(toks, phrase("c d"), concatenator = character()),
               "The length of concatenator must be 1")
  
  # update concatenator even without matches
  toks4 <- tokens_compound(toks, phrase("xxxx yyy"), concatenator = "++")
  expect_equal(meta(toks4, field = "concatenator", type = "object"), " ")
  expect_equal(sort(attr(toks4, "types")),
               sort(c("a", "b", "c", "d", "e", "f", "g")))
 
})

test_that("tokens_compound works with nested tokens", {
    
    toks <- tokens("a b c d")
    pat <- phrase(c("a b", "b c", "a b c"))
    expect_equal(
        as.character(tokens_compound(toks, pat, join = FALSE)),
        c("a_b_c", "d")
    )
    expect_equal(
        as.character(tokens_compound(toks, pat, join = TRUE)),
        c("a_b_c", "d")
    )
})

test_that("tokens_compound works with nested and overlapping tokens", {
    
    toks <- tokens("a b c d e")
    pat <- phrase(c("a b", "a b c", "c d"))
    expect_equal(
        as.character(tokens_compound(toks, pat, join = FALSE)),
        c("a_b_c", "c_d", "e")
    )
    expect_equal(
        as.character(tokens_compound(toks, pat, join = TRUE)),
        c("a_b_c_d", "e")
    )
})

test_that("tokens_compound works with dictionaries", {
    dict <- dictionary(list(taxcgt = c("capital gains tax*"), taxit = "inheritance tax*"))
    toks <- tokens("The new law included capital gains taxes and inheritance taxes.")
    expect_equal(
        as.character(tokens_compound(toks, dict))[c(5, 7)],
        c("capital_gains_taxes", "inheritance_taxes")
    )
    expect_equal(
        tokens_compound(toks, dict),
        tokens_compound(toks, phrase(dict))
    )

    dict <- dictionary(list(tax1 = c("capital gains", "taxes"),
                            tax2 = "gains taxes"))
    expect_equal(
        as.character(tokens_compound(toks, dict, join = TRUE))[5],
        c("capital_gains_taxes")
    )
    expect_equal(
        as.character(tokens_compound(toks, dict, join = FALSE))[5:6],
        c("capital_gains", "gains_taxes")
    )
})

test_that("tokens_compound error when dfm is given, #1006", {
    toks <- tokens("a b c")
    expect_error(tokens_compound(toks, dfm(tokens("b c d"))))
})

test_that("tokens_compound window is working", {

  txt <- c("a b c d e f g")
  toks <- tokens(txt)
  pat <- phrase(c("a b", "c d", "g"))

  expect_equal(
    as.list(tokens_compound(toks, pat, join = TRUE)),
    as.list(tokens(c("a_b c_d e f g")))
  )
  expect_equal(
    as.list(tokens_compound(toks, pat, join = FALSE)),
    as.list(tokens(c("a_b c_d e f g")))
  )

  expect_equal(
    as.list(tokens_compound(toks, pat, join = TRUE, window = 1)),
    as.list(tokens(c("a_b_c_d_e f_g")))
  )
  expect_equal(
    as.list(tokens_compound(toks, pat, join = FALSE, window = 1)),
    as.list(tokens(c("a_b_c b_c_d_e f_g")))
  )

  expect_equal(
    as.list(tokens_compound(toks, pat, join = TRUE, window = c(1, 0))),
    as.list(tokens(c("a_b_c_d e f_g")))
  )
  expect_equal(
    as.list(tokens_compound(toks, pat, join = FALSE, window = c(1, 0))),
    as.list(tokens(c("a_b b_c_d e f_g")))
  )

  expect_equal(
    as.list(tokens_compound(toks, pat, join = TRUE, window = c(0, 1))),
    as.list(tokens(c("a_b_c_d_e f g")))
  )
  expect_equal(
    as.list(tokens_compound(toks, pat, join = FALSE, window = c(0, 1))),
    as.list(tokens(c("a_b_c c_d_e f g")))
  )

  expect_equal(
    as.list(tokens_compound(toks, pat, join = TRUE, window = c(100, 100))),
    as.list(tokens(c("a_b_c_d_e_f_g")))
  )
  expect_error(
    tokens_compound(toks, pat, join = FALSE, window = -1),
    "The value of window must be between 0 and Inf"
  )
  expect_error(
    tokens_compound(toks, pat, join = FALSE, window = c(1, 1, 2)),
    "The length of window must be between 1 and 2"
  )
  expect_equal(
    as.list(tokens_compound(tokens_remove(toks, "a", padding = TRUE), pat, join = TRUE, window = 1)),
    list(text1 = c("", "b_c_d_e", "f_g"))
  )
  expect_equal(
    as.list(tokens_compound(tokens_remove(toks, "a", padding = TRUE), pat, join = FALSE, window = 1)),
    list(text1 = c("", "b_c_d_e", "f_g"))
  )
  toks_pad1 <- tokens_remove(toks, "e", padding = TRUE)
  expect_equal(
    as.list(tokens_compound(toks_pad1, pat, join = TRUE, window = 1)),
    list(text1 = c("a_b_c_d", "", "f_g"))
  )
  expect_equal(
    as.list(tokens_compound(toks_pad1, pat, join = FALSE, window = 1)),
    list(text1 = c("a_b_c", "b_c_d", "", "f_g"))
  )

  expect_equal(
    as.list(tokens_compound(toks_pad1, pat, join = TRUE, window = 2)),
    list(text1 = c("a_b_c_d", "", "f_g"))
  )
  expect_equal(
    as.list(tokens_compound(toks_pad1, pat, join = FALSE, window = 2)),
    list(text1 = c("a_b_c_d", "", "f_g"))
  )

  toks_pad2 <- tokens_remove(toks, c("a", "e"), padding = TRUE)
  expect_equal(
    as.list(tokens_compound(toks_pad2, pat, join = TRUE, window = 1)),
    list(text1 = c("", "b_c_d", "", "f_g"))
  )
  expect_equal(
    as.list(tokens_compound(toks_pad2, pat, join = FALSE, window = 1)),
    list(text1 = c("", "b_c_d", "", "f_g"))
  )

})

test_that("tokens_compound ignores padding", {
    
    toks <- tokens("a b c d e")
    toks <- tokens_remove(toks, "b", padding = TRUE)
    
    toks_comp1 <- tokens_compound(toks, list(c("", "c"), c("d", "e")))
    expect_equal(
        as.character(toks_comp1),
        c("a", "", "c", "d_e")
    )
    
    col <- data.frame(collocation = c(" c", "d e"))
    class(col) <- c("collocations", "textstat", "data.frame")
    toks_comp2 <- tokens_compound(toks, col)
    expect_equal(
        as.character(toks_comp2),
        c("a", "", "c", "d_e")
    )
})


test_that("apply_if argument is working", {
    
    dat <- data.frame(text = c("C++ is a language",
                               "+++ Section C +++"),
                      topic = c("language", "separator"))
    corp <- corpus(dat)
    toks <- tokens(corp)
    
    toks1 <- tokens_compound(toks, phrase("+ +"), concatenator = "",
                             apply_if = toks$topic == "language")
    expect_identical(
        as.list(toks1),
        list(text1 = c("C", "++", "is", "a", "language"),
             text2 = c("+", "+", "+", "Section", "C", "+", "+", "+"))
    )
    
    toks2 <- tokens_compound(toks, "c", window = c(0, 2), concatenator = "",
                             apply_if = toks$topic == "language") %>% 
             tokens_select(min_nchar = 3)
    expect_identical(
        as.list(toks2),
        list(text1 = c("C++", "language"),
             text2 = c("Section"))
    )

})
