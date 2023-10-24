test_that("test phrase for character", {
    txt1 <- c("capital gains tax", "one two", "three")
    expect_equivalent(
        phrase(txt1),
        list(c("capital", "gains", "tax"), c("one", "two"), "three")
    )
    
    txt2 <- c("capital_gains_tax", "one_two", "three")
    expect_equivalent(
        phrase(txt2, separator = "_"),
        list(c("capital", "gains", "tax"), c("one", "two"), "three")
    )
    expect_error(phrase(txt2, separator = "__"),
                 "The value of separator must be 1 character")
    expect_error(phrase(txt2, separator = c("_", " ")),
                 "The length of separator must be 1")
    
    expect_equivalent(
        phrase(letters),
        as.list(letters)
    )
})

test_that("test phrase for dictionaries", {
    dict1 <- dictionary(list(country = c("United States"),
                            institution = c("Congress", "feder* gov*")))
    expect_equivalent(
        phrase(dict1),
        list(c("united", "states"), c("congress"), c("feder*", "gov*"))
    )
    dict2 <- dictionary(list(country = c("United+States"),
                            institution = c("Congress", "feder*+gov*")),
                        separator = "+")
    expect_equivalent(
        phrase(dict2),
        list(c("united", "states"), c("congress"), c("feder*", "gov*"))
    )
})

test_that("test as.phrase", {
    toks <- tokens(c("United States", "Congress", "federal government"))
    # expect_equivalent(
    #     as.phrase(toks),
    #     list(c("United", "States"), "Congress", c("federal", "government"))
    # )
    # expect_equivalent(
    #     suppressWarnings(phrase(toks)),
    #     list(c("United", "States"), "Congress", c("federal", "government"))
    # )
    # lis <- as.list(toks)
    # expect_equivalent(
    #     as.phrase(lis),
    #     list(c("United", "States"), "Congress", c("federal", "government"))
    # )
    load("../data/collocations/col.rda")
    expect_equivalent(
        as.phrase(col),
        list(c("a", "b"), c("b", "c"), c("c", "d"), c("a", "b", "c"))
    )
    expect_equivalent(
        suppressWarnings(phrase(col)),
        list(c("a", "b"), c("b", "c"), c("c", "d"), c("a", "b", "c"))
    )
    dict <- dictionary(list(country = c("United+States"),
                            institution = c("Congress", "feder*+gov*")),
                        separator = "+")
    expect_equivalent(
      phrase(dict),
      list(c("united", "states"), c("congress"), c("feder*", "gov*"))
    )
})

test_that("helper functions for phrase() work", {
    p <- phrase(c("capital gains tax", "one two", "three"))
    expect_identical(
        quanteda:::as.list.phrases(p),
        list(c("capital", "gains", "tax"), c("one", "two"), "three")
    )
    expect_output(
        print(p)
    )
    expect_identical(
        is.phrase(list(c("capital", "gains", "tax"), c("one", "two"), "three")),
        FALSE
    )
    expect_identical(
        is.phrase(p),
        TRUE
    )
})
