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

test_that("features2list works as expected", {
    
    target <- list(c("United", "States"), "Congress", c("feder*", "gov*"))
    # character
    expect_equal(quanteda:::features2list(c("United States", "Congress", "feder* gov*")),
                 target)
    # list
    expect_equal(quanteda:::features2list(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))),
                 target)
    # tokens
    expect_equal(quanteda:::features2list(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")),
                 target)
    # dictionary
    expect_equal(quanteda:::features2list(dictionary(list(country = c("United States"), 
                                                          institution = c("Congress", "feder* gov*")), 
                                                     tolower = FALSE)),
                 target)
    
    expect_equal(quanteda:::features2list(dictionary(list(country = c("United+States"), 
                                                          institution = c("Congress", "feder*+gov*")), 
                                                     tolower = FALSE, concatenator = '+')),
                 target)
    
    # collocations
    colls <- textstat_collocations(tokens(c("United States", "Congress", "federal government")), min_count = 1, method = "lr")
    expect_equal(quanteda:::features2list(colls),
                 list(c("United", "States"), c("federal", "government")))
})

test_that("features2vector works as expected", {
    
    # character
    expect_warning(quanteda:::features2vector(c("United States", "Congress", "feder* gov*")))
    expect_silent(quanteda:::features2vector(c("America", "Congress", "gov*")))
                   
    # list
    expect_warning(quanteda:::features2vector(list(c("United", "States"), c("Congress"), c("feder*", "gov*"))))
    expect_silent(quanteda:::features2vector(list("America", "Congress", "gov*")))
                   
    # tokens
    expect_warning(quanteda:::features2vector(tokens(c("United States", "Congress", "feder* gov*"), what = "fasterword")))
    expect_silent(quanteda:::features2vector(tokens(c("America", "Congress", "gov*"), what = "fasterword")))
    
    # dictionary
    expect_warning(quanteda:::features2vector(dictionary(list(country = c("United States"), 
                                                              institution = c("Congress", "feder* gov*")), tolower = FALSE)))
    expect_silent(quanteda:::features2vector(dictionary(list(country = c("America"), 
                                                              institution = c("Congress", "gov*")), tolower = FALSE)))
    
    # collocations
    colls <- textstat_collocations(tokens(c("United States", "Congress", "federal government")), min_count = 1)
    expect_warning(quanteda:::features2vector(colls))
})



