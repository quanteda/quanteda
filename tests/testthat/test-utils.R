context("test utils")

test_that("message_select works as expected", {
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        "removed 10 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 0, 0),
        " and 5 documents"
    )
    
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 0),
        "removed 1 feature"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 0, 0),
        " and 1 document$"
    )
    expect_message(
        quanteda:::message_select("remove", 0, 0, 0, 0),
        "removed 0 features$"
    )

    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        "removed 1,000 features"
    )
    expect_message(
        quanteda:::message_select("select", 1000, 1000000, 0, 0),
        " and 1,000,000 documents$"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "2 features"
    )
    expect_message(
        quanteda:::message_select("remove", 10, 5, 2, 3),
        "3 documents"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        ", padded"
    )
    expect_message(
        quanteda:::message_select("remove", 1, 5, 0, 1),
        "1 document"
    )
    expect_message(
        quanteda:::message_select("remove", 5, 1, 1, 0),
        "1 feature"
    )
})

test_that("pipes work", {
    expect_true(!"package:magrittr" %in% search())
    expect_equal(
        tokens(char_tolower("A B C")),
        tokens("A B C") %>% tokens_tolower()
    )
})

# test_that("features2list works as expected", {
#     
#     target <- list(c("United", "States"), "Congress", c("feder*", "gov*"))
#     # character
#     expect_equal(quanteda:::features2list(c("United States", "Congress", "feder* gov*")),
#                  as.list(c("United States", "Congress", "feder* gov*")))
#     expect_equivalent(phrase(c("United States", "Congress", "feder* gov*")),
#                       target)
#     # list
#     expect_equal(quanteda:::features2list(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))),
#                  list(c("United", "States"), c("Congress"), c("feder*", "gov*")))
#     expect_equivalent(phrase(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))),
#                  list(c("United", "States"), c("Congress"), c("feder*", "gov*")))
#     
#     # tokens
#     expect_equal(quanteda:::features2list(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")),
#                  target)
#     expect_equivalent(phrase(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")),
#                       target)
#     # dictionary
#     dict <- dictionary(list(country = c("United States"), 
#                             institution = c("Congress", "feder* gov*")), 
#                        tolower = FALSE)
#     expect_equal(quanteda:::features2list(dict),
#                  list(c("United States"), "Congress", c("feder* gov*")))
#     expect_equivalent(phrase(dict), target)
# 
#     # collocations
#     colls <- textstat_collocations(tokens(c("United States", "Congress", "federal government")), min_count = 1, method = "lr")
#     expect_equal(quanteda:::features2list(colls),
#                  list(c("federal government"), c("United States")))
#     expect_equivalent(phrase(colls),
#                       list(c("federal", "government"), c("United", "States")))
# })
# 
# test_that("features2vector works as expected", {
#     
#     # character
#     expect_silent(quanteda:::features2vector(c("United States", "Congress", "feder* gov*")))
#     expect_silent(quanteda:::features2vector(c("America", "Congress", "gov*")))
#                    
#     # list
#     # expect_warning(quanteda:::features2vector(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))))
#     expect_silent(quanteda:::features2vector(list("America", "Congress", "gov*")))
#                    
#     # tokens
#     #expect_warning(quanteda:::features2vector(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")))
#     expect_silent(quanteda:::features2vector(tokens(c("America", "Congress", "gov*"), what = "fasterword")))
#     
#     # dictionary
#     expect_silent(quanteda:::features2vector(dictionary(list(country = c("United States"), 
#                                                              institution = c("Congress", "feder* gov*")), tolower = FALSE)))
#     expect_silent(quanteda:::features2vector(dictionary(list(country = c("America"), 
#                                                               institution = c("Congress", "gov*")), tolower = FALSE)))
#     
#     # collocations
#     colls <- textstat_collocations(tokens(c("United States", "Congress", "federal government")), min_count = 1)
#     # expect_warning(quanteda:::features2vector(colls))
# })



