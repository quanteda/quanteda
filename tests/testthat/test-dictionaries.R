context("dictionary construction")

test_that("dictionary constructors fail if all elements unnamed: explicit", {
    expect_error(dictionary(list(c("a, b"), "C")),
                 "dictionary elements must be named")
    expect_error(dictionary(list(first =  c("a, b"), "C")),
                 "unnamed dictionary entry: C")
})

test_that("dictionary constructors fail if all elements unnamed: implicit", {
    expect_error(dictionary(c("a, b"), "C"),
                 "dictionary elements must be named")
    expect_error(dictionary(first =  c("a, b"), "C"),
                 "unnamed dictionary entry: C")
})

test_that("dictionary constructors fail if a value is numeric", {
    expect_error(dictionary(list(first =  c("a, b"), second = 2016)),
                 "non-character entries found: 2016")
})

test_that("dictionary constructor works on list explicitly or implicitly", {
    expect_equal(dictionary(list(first =  c("a, b"), second = "c")),
                 dictionary(first =  c("a, b"), second = "c"))
})

marydict <- dictionary("A CATEGORY" = c("more", "lamb", "little"),
                       "ANOTHER CATEGORY" = c("had", "mary"))

test_that("dictionary constructor works with wordstat format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.cat"),
                      marydict)
})

test_that("dictionary constructor works with Yoshikoder format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.ykd"),
                      marydict)
})

test_that("dictionary constructor works with YAML format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.yml"),
                      marydict)
})

test_that("dictionary constructor works with LIWC format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.dic"),
                      dictionary("A_CATEGORY" = c("lamb", "little", "more"),
                                 "ANOTHER_CATEGORY" = c("had", "mary")))
})

test_that("dictionary constructor works with Lexicoder format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.lcd", toLower = FALSE),
                      dictionary("A category" = c("more", "lamb", "little"),
                                 "Another category" = c("had", "mary")))
})

test_that("read a dictionary with NA as a key", {
    testdict <- dictionary(file = "../data/dictionaries/issue-459.cat")
    # this is not really the best test, since the name of entry 3 is ".NA",
    # when it should be SOUTH.NA.  This seems to be a limitation of 
    # readWStatDict() in dictionaries.R
    expect_true(grepl("NA", names(testdict)[3]))
})

