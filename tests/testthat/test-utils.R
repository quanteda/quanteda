test_that("Test quanteda:::mktemp function for test dirs",{
    filename <- quanteda:::mktemp()
    expect_true(file.exists(filename))
    filename2 <- quanteda:::mktemp()
    expect_true(file.exists(filename2))
    expect_false(filename == filename2)
    
    # test directory parameter
    dirname <- quanteda:::mktemp(directory=T)
    expect_true(dir.exists(dirname))
    
    # test prefix parameter
    filename <- quanteda:::mktemp(prefix='testprefix')
    expect_equal(
        substr(basename(filename), 1, 10),
        'testprefix'
    )
    
    # test that a new filename will be given if the original already exists
    set.seed(0)
    original_filename <- quanteda:::mktemp()
    set.seed(0)
    new_filename <- quanteda:::mktemp()
    expect_false(original_filename == new_filename)
    expect_true(file.exists(original_filename))
    expect_true(file.exists(new_filename))
})

test_that("sequence2list works as expected", {
    
    target <- list(c("United", "States"), "Congress", c("feder*", "gov*"))
    # character
    expect_equal(quanteda:::sequence2list(c("United States", "Congress", "feder* gov*")),
                 target)
    # list
    expect_equal(quanteda:::sequence2list(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))),
                 target)
    # tokens
    expect_equal(quanteda:::sequence2list(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")),
                 target)
    # dictionary
    expect_equal(quanteda:::sequence2list(dictionary(list(country = c("United States"), 
                                                          institution = c("Congress", "feder* gov*")), tolower = FALSE)),
                 target)
    # collocations
    collocs <- textstat_collocations(tokens(c("United States", "Congress", "federal government")), min_count = 1)
    expect_equal(quanteda:::sequence2list(collocs),
                 list(c("United", "States"), c("federal", "government")))
})


test_that("deprecate_arguments works as expected", {
    fn <- function(remove_numbers = TRUE, ...) {
        args <- as.list(match.call())
        remove_numbers <- quanteda:::deprecate_argument("removeNumbers", "remove_numbers", args)
        cat("remove_numbers =", remove_numbers, "\n")
    }
    expect_warning(
        fn(removeNumbers = FALSE),
        "argument \"removeNumbers\" is deprecated: use \"remove_numbers\" instead."
    )
    expect_output(
        fn(remove_numbers = FALSE),
        "remove_numbers = FALSE"
    )
    
    # for tokens
    expect_identical(
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE)),
        c("This", "punctuation")
    )
    expect_identical(
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE)),
        as.character(tokens(c(d1 = "This: punctuation"), remove_punct = TRUE))
    )
    expect_warning(
        as.character(tokens(c(d1 = "This: punctuation"), removePunct = TRUE)),
        "argument \"removePunct\" is deprecated: use \"remove_punct\" instead."
    )    
    expect_warning(
        tokens(c(d1 = "This: punctuation"), notanargument = TRUE),
        "Argument notanargument not used"
    )
    
})

test_that("deprecate arguments works with values from parent frame", {
    fn <- function(remove_numbers = TRUE, ...) {
        args <- as.list(match.call())
        args <- lapply(args, function(a) eval(a, enclos = parent.frame()))
        remove_numbers <- quanteda:::deprecate_argument("removeNumbers", "remove_numbers", args)
        cat("remove_numbers =", remove_numbers, "\n")
    }
    tmp = FALSE
    
    expect_output(
        fn(remove_numbers = tmp),
        "remove_numbers = FALSE"
    )
    expect_output(
        fn(removeNumbers = tmp),
        "remove_numbers = FALSE"
    )
})
