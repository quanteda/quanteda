context("test tokens_compound")

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

    txts <- "we like high quality sound"
    seqs <- phrase(c("high quality", "quality sound"))
    expect_equal(as.list(tokens_compound(tokens(txts), seqs, join = TRUE)),
                      list(text1 = c("we", "like", "high_quality_sound")))
    expect_equal(as.list(tokens_compound(tokens(txts), seqs, join = FALSE)),
                      list(text1 = c("we", "like", "high_quality", "quality_sound")))

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

test_that("tokens_compound works as expected with nested tokens", {

    expect_equal(
        as.character(tokens_compound(tokens("a b c d"), phrase(c("a b", "a b c")),
                     join = FALSE)),
        c("a_b_c", "d")
    )
    expect_equal(
        as.character(tokens_compound(tokens("a b c d"), phrase(c("a b", "a b c")),
                     join = TRUE)),
        c("a_b_c", "d")
    )
})

test_that("tokens_compound works as expected with nested and overlapping tokens", {

    expect_equal(
        as.character(tokens_compound(tokens("a b c d e"),
                                     phrase(c("a b", "a b c", "c d")),
                                     join = FALSE)),
        c("a_b_c", "c_d", "e")
    )
    expect_equal(
        as.character(tokens_compound(tokens("a b c d e"),
                                     phrase(c("a b", "a b c", "c d")),
                                     join = TRUE)),
        c("a_b_c_d", "e")
    )
})

test_that("tokens_compound works as expected with collocations", {

    toks <- tokens("The new law included capital gains taxes and inheritance taxes.")
    cols <- data.frame(collocation = c("the new", "capital gains", "gains taxes"), stringsAsFactors = FALSE)
    class(cols) <- c("collocations", "data.frame")

    expect_true(all(
      c("The_new", "capital_gains", "gains_taxes") %in%
      as.character(tokens_compound(toks, phrase(cols), join = FALSE))
    ))

    expect_true(all(
      c("The_new", "capital_gains_taxes") %in%
        as.character(tokens_compound(toks, phrase(cols), join = TRUE))
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
         tokens_compound(toks, phrase(cols))
     )
    expect_equal(
        tokens_compound(toks, cols, join = TRUE),
        tokens_compound(toks, phrase(cols), join = TRUE)
    )
})

test_that("tokens_compound works as expected with dictionaries", {
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
    expect_error(tokens_compound(toks, dfm("b c d")))
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
    "window sizes cannot be negative"
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
    list(text1 = c("a_b_c_d", "a_b_c_d", "", "f_g"))
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
