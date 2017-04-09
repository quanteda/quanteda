context("testing tokens")

##
## tokens_hashed tests
##

test_that("nsyllable works as expected", {
    txt <- c(one = "super freakily yes",
             two = "merrily all go aerodynamic")
    toksh <- tokens(txt)
    toks <- tokenize(txt)
    expect_equivalent(nsyllable(toks), nsyllable(toksh), list(c(2, 3, 1), c(3, 1, 1, 5)))
})

test_that("nsyllable works as expected with padding = TRUE", {
    txt <- c(one = "super freakily yes",
             two = "merrily, all go aerodynamic")
    toks <- tokens_remove(tokens(txt), c("yes", "merrily"), padding = TRUE)
    expect_equivalent(nsyllable(toks), list(c(2, 3, NA), c(NA, NA, 1, 1, 5)))
})


test_that("tokens_wordstem works as expected for tokens_hashed", {

    txt <- c(one = "Eating eater eaters eats ate.",
             two = "Taxing taxes taxed my tax return.")
    toks <- tokenize(char_tolower(txt), remove_punct = TRUE)
    toksh <- tokens(char_tolower(txt), remove_punct = TRUE)
    classic <- tokens_wordstem(toks)
    hashed <- tokens_wordstem(toksh)
    expect_equivalent(classic, as.tokenizedTexts(hashed))
})

test_that("ngrams works as expected for tokens_hashed", {
    txt <- c(one = char_tolower("Insurgents killed in ongoing fighting."),
             two = "A B C D E")
    toks <- tokenize(txt, remove_punct = TRUE)
    toksh <- tokens(txt, remove_punct = TRUE)
    classic <- tokens_ngrams(toks, n = 2:3)
    hashed <- as.tokenizedTexts(tokens_ngrams(toksh, n = 2:3))
    # testthat::expect_equivalent(as.list(classic),
    #                             as.list(hashed))
    classic <- list(sort(unlist(classic$one)), sort(unlist(classic$two)))
    hashed <- list(sort(unlist(hashed$one)), sort(unlist(hashed$two)))
    expect_equivalent(lapply(classic, sort),
                      lapply(hashed, sort))
})

test_that("skipgrams works as expected for tokens_hashed", {
    txt <- c(one = "insurgents killed in ongoing fighting")
    toks <- tokenize(txt)
    toksh <- tokens(txt)
    classic <- skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")
    hashed <- skipgrams(toksh, n = 3, skip = 0:2, concatenator = " ")
    expect_equivalent(classic, as.tokenizedTexts(hashed))
})


test_that("as.tokens tokenizedTexts works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    toks <- tokenize(txt)
    toksh <- tokens(txt) 
    expect_equivalent(toksh, 
                      as.tokens(toks))
})

test_that("as.tokens list version works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    toksh <- tokens(txt) 
    toks <- tokenize(txt)
    attributes(toks) <- NULL
    names(toks) <- names(txt)
    expect_equal(class(toks), "list")
    expect_equivalent(toksh, 
                      as.tokens(toks))
})

test_that("as.tokens list version works as expected", {
    txt <- c(doc1 = "The first sentence is longer than the second.",
             doc2 = "Told you so.")
    tokslist <- as.list(tokens(txt))
    toks <- tokens(txt)
    expect_equal(as.tokens(tokslist), 
                      toks)
})


test_that("tokens indexing works as expected", {
    toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))

    expect_equal(toks$d1, c("one", "two", "three"))
    expect_equal(toks[[1]], c("one", "two", "three"))
    
    expect_equal(as.list(toks["d2"]), list(d2 = c("four", "five", "six")))
    expect_equal(as.list(toks[2]), list(d2 = c("four", "five", "six")))
    
    # issue #370
    expect_equal(attr(toks[1], "types"), c("one", "two", "three"))
    expect_equal(attr(toks[2], "types"), c("four", "five", "six"))
})

test_that("tokens_hashed_recompile combine duplicates is working", {
    toksh <- tokens(c(one = "a b c d A B C D", two = "A B C d"))
    expect_equivalent(attr(toksh, "types"),
                    c("a", "b", "c", "d", "A", "B", "C", "D"))
    expect_equivalent(attr(tokens_tolower(toksh), "types"),
                    c("a", "b", "c", "d"))
    attr(toksh, "types") <- char_tolower(attr(toksh, "types"))
    expect_equivalent(attr(quanteda:::tokens_hashed_recompile(toksh), "types"),
                    c("a", "b", "c", "d"))
    
})

test_that("test `ngrams` with padding = FALSE: #428", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks2 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    
    expect_equal(as.list(tokens_ngrams(toks2, n = 2)),
                 list(doc1 = c("a_c", "c_d", "d_f", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 3)),
                 list(doc1 = c("a_c_d", "c_d_f", "d_f_g")))
    expect_equal(as.list(tokens_ngrams(toks2, n = 2, skip = 2)),
                 list(doc1 = c("a_f", "c_g")))
})

test_that("test `ngrams` with padding = TRUE: #428", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    
    expect_equal(as.list(tokens_ngrams(toks3, n = 2)),
                 list(doc1 = c("c_d", "f_g")))
    expect_equal(as.list(tokens_ngrams(toks3, n = 3)),
                 list(doc1 = character(0)))
    expect_equal(as.list(tokens_ngrams(toks3, n = 2, skip = 2)),
                 list(doc1 = c("a_d", "c_f", "d_g")))
})

test_that("test dfm with padded tokens, padding = FALSE", {
    toks <- tokens(c(doc1 = 'a b c d e f g',
                     doc2 = 'a b c g',
                     doc3 = ''))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(1, 1, 1, 1, 1, 
                               1, 1, 0, 0, 1,
                               0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("test dfm with padded tokens, padding = TRUE", {
    toks <- tokens(c(doc1 = 'a b c d e f g',
                     doc2 = 'a b c g',
                     doc3 = ''))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equivalent(as.matrix(dfm(toks3)),
                      matrix(c(2, 1, 1, 1, 1, 1, 
                               1, 1, 1, 0, 0, 1, 
                               0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE))
})

test_that("test verious functions with padded tokens, padding = FALSE", {
    toks <- tokens(c(doc1 = 'A b c d E f g',
                     doc2 = 'a b c g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = FALSE)
    expect_equivalent(nfeature(toks3), 6)
    expect_equivalent(nfeature(tokens_tolower(toks3)), 5)
    expect_equivalent(nfeature(tokens_toupper(toks3)), 5)
    expect_equivalent(as.character(toks3),
                      c("A", "c", "d", "f", "g", "a", "c", "g"))
})

test_that("test verious functions with padded tokens, padding = TRUE", {
    toks <- tokens(c(doc1 = 'A b c d E f g',
                     doc2 = 'a b c g'))
    toks3 <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    expect_equivalent(nfeature(toks3), 7)
    expect_equivalent(nfeature(tokens_tolower(toks3)), 6)
    expect_equivalent(nfeature(tokens_toupper(toks3)), 6)
    expect_equivalent(as.character(toks3),
                      c("A", "", "c", "d", "", "f", "g", "a", "", "c", "g"))
})

test_that("docnames works for tokens", {
    expect_equal(names(data_char_ukimmig2010),
                 docnames(tokens(data_char_ukimmig2010)))
})

test_that("longer features longer than documents do not crash (#447)", {
    toks <- tokens(c(d1 = 'a b', d2 = 'a b c d e'))
    feat <- 'b c d e'
    # bugs in C++ needs repeated tests
    expect_silent(replicate(10, tokens_select(toks, feat)))
    expect_equal(
        as.list(tokens_select(toks, feat)),
        list(d1 = character(0), d2 = c("b", "c", "d", "e"))
    )
})

test_that("tokens works as expected for what = \"character\"", {
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = TRUE)),
        c("o", "n", "e", ",", "t", "w", "o", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_separators = FALSE)),
        c("o", "n", "e", ",", " ", "t", "w", "o", " ", "t", "h", "r", "e", "e", ".")
    )
    expect_equal(
        as.character(tokens("one, two three.", what = "character", remove_punct = TRUE)),
        c("o", "n", "e", "t", "w", "o", "t", "h", "r", "e", "e")
    )
})

test_that("tokens works with unusual hiragana #554", {
    skip_on_travis()
    skip_on_cran()
    skip_on_appveyor()
    skip_on_os("windows")
    txts <- c("づいﾞ", "゛んﾞ", "たーﾟ")
    expect_equivalent(as.list(tokens(txts)),
                      list(c('づ', 'いﾞ'), c('゛', 'んﾞ'), c('た', 'ーﾟ')))
})

test_that("types attribute is a character vector", {
    toks <- tokens("one two three")
    expect_true(is.character(attr(toks, 'types')))
    expect_equal(length(attributes(attr(toks, 'types'))), 0)
})


#' # coerce an object into a tokens class
#' as.tokens(toks)


#' \dontrun{
#' tokens2 <- tokenize(head(inaugTexts, 10), remove_punct=TRUE)
#' tokens2_hashed <- hashTokens(tokens2)
#' 
#' profvis::profvis({
#' ngrams(tokens2_hashed, n = 2:3, skip = 1:2, concatenator = "-")
#' })
#' 
#' microbenchmark::microbenchmark(
#'  old=ngrams(tokens2, n = 2:3, skip = 1:2, concatenator = "-"),
#'  new=ngrams(tokens2_hashed, n = 2:3, skip = 1:2, concatenator = "-"),
#'  times=10, unit='relative'
#' )
#' 
#' 
#' Rcpp::sourceCpp('src/ngrams_hashed.cpp')
#' Rcpp::sourceCpp('src/ngrams_class.cpp')
#' Rcpp::sourceCpp('src/ngrams.cpp')
#' nm <- new(ngramMaker)
#'
#' microbenchmark::microbenchmark(
#'    old=skipgramcpp(tokens2[[1]], 2:3, 1:2, '-'),
#'    new=qatd_cpp_ngram_hashed_vector(tokens2_hashed[[1]], 2:3, 1:2),
#'    class=nm$generate(tokens2_hashed[[1]], 2:3, 1:2),
#'    times=100, unit='relative'
#' )
#' 
#' microbenchmark::microbenchmark(
#'  obj=nm$generate_list(tokens2_hashed, 2:3, 1:2),
#'  ptr=nm$generate_list_ptr(tokens2_hashed, 2:3, 1:2),
#'  times=100, unit='relative'
#' )
#' 
#' 
#' tokens3 <- rep(letters, 50)
#' types3 <- unique(tokens3)
#' tokens3_hashed <- match(tokens3, types3)
#' microbenchmark::microbenchmark(
#'    old=skipgramcpp(tokens3, 2:3, 1:2, '-'),
#'    new=qatd_cpp_ngram_hashed_vector(tokens3_hashed, 2:3, 1:2),
#'    times=10, unit='relative'
#'  )
#' 
#' # Test with greater lexical diversity
#' tokens4 <- paste0(sample(letters, length(tokens3), replace=TRUE), 
#'                   sample(letters, length(tokens3), replace=TRUE))
#' types4 <- unique(tokens4)
#' tokens4_hashed <- match(tokens4, types4)
#' microbenchmark::microbenchmark(
#'    low=qatd_cpp_ngram_hashed_vector(tokens3_hashed, 2:3, 1:2),
#'    high=qatd_cpp_ngram_hashed_vector(tokens4_hashed, 2:3, 1:2),
#'    times=100, unit='relative'
#' )
#' 
#' 
#' # Comparison with tokenizers's skip-grams
#' tokenizers::tokenize_skip_ngrams('a b c d e', n=3, k=1) 
#' # "a c e" "a b c" "b c d" "c d e"
#' tokenizers::tokenize_skip_ngrams('a b c d e', n=3, k=2) 
#' # "a c e" "a b c" "b c d" "c d e"
#' 
#' ngrams(tokenize('a b c d e'), n=3, skip=0:1, concatenator=' ') 
#' # "a b c" "a b d" "a c d" "a c e" "b c d" "b c e" "b d e" "c d e"
#' 
#'}


test_that("remove_url works as expected", {
    txt <- c("The URL was http://t.co/something.",
             "The URL was http://quanteda.io",
             "https://github.com/kbenoit/quanteda/issue/1 is another URL")
    toks <- tokens(txt, remove_url = TRUE)
    expect_equal(
        as.list(toks),
        list(c("The", "URL", "was"), c("The", "URL", "was"), c("is", "another", "URL"))
    )

    toks2 <- tokenize(txt, remove_url = TRUE)
    expect_equivalent(
        as.list(toks2),
        list(c("The", "URL", "was"), c("The", "URL", "was"), c("is", "another", "URL"))
    )
})

test_that("remove_punct and remove_twitter interact correctly, #607", {
    txt <- "they: #stretched, @ @@ in,, a # ## never-ending @line."
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = TRUE, remove_twitter = TRUE)),
        c("they", "stretched", "in", "a", "never-ending", "line")
    )    
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = FALSE)),
        c("they", ":", "#stretched", ",", "@", "@@", "in", ",", ",", "a", "#", "##", "never", "-", "ending", "@line", ".")
    )    
    # this is #607
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = TRUE, remove_twitter = FALSE)),
        c("they", "#stretched", "in", "a", "never-ending", "@line")
    )
    # remove_twitter should be inactive if remove_punct is FALSE
    expect_equal(
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = TRUE)),
        as.character(tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = FALSE))
    )
    expect_warning(
        tokens(txt, what = "word", remove_punct = FALSE, remove_twitter = TRUE),
        "remove_twitter reset to FALSE when remove_punct = FALSE"
    )
})

test_that("+ operator works with tokens", {
    
    txt1 <- c(d1 = "This is sample document one.",
              d2 = "Here is the second sample document.")
    txt2 <- c(d3 = "And the third document.")
    
    toks_added <- tokens(txt1) + tokens(txt2)
    expect_equal(ntype(toks_added), length(attr(toks_added, "types")))
    expect_equal(ndoc(toks_added), 3)
    
    ## THIS BEHAVIOUR SHOULD NOT DUPLICATE DOCUMENT NAMES - SEE TEST EXPECTATIONS
    txt3 <- c(d1 = "new words")
    toks_added2 <- tokens(txt1) + tokens(txt3)
    expect_equal(ntype(toks_added2), length(attr(toks_added2, "types")))
    expect_equal(
        toks_added2[[1]],
        c("This", "is", "sample", "document", "one", ".", "new", "words")       
    )
    expect_equal(ndoc(toks_added2), 2)

})

test_that("c() works with tokens", {

    txt1 <- c(d1 = "This is sample document one.",
              d2 = "Here is the second sample document.")
    txt2 <- c(d3 = "And the third document.")
    txt3 <- c(d4 = "This is sample document 4.")
    txt4 <- c(d1 = "This is sample document five!")
    
    expect_equal(
        c(tokens(txt1), tokens(txt2)),
        tokens(txt1) + tokens(txt2)
    )
    
    expect_equal(
        c(tokens(txt1), tokens(txt2), tokens(txt3)),
        tokens(txt1) + tokens(txt2) + tokens(txt3)
    )
    
    expect_error(
        c(tokens(txt1), tokens(txt4)),
        'Document names are duplicated'
    )
})

test_that("docvars are erased for tokens added", {
    mycorpus <- corpus(c(d1 = "This is sample document one.",
                         d2 = "Here is the second sample document."), 
                        docvars = data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2)))
    expect_equivalent(
        docvars(tokens(mycorpus, include_docvars = TRUE)),
        data.frame(dvar1 = c("A", "B"), dvar2 = c(1, 2))
    )
    expect_equivalent(
        docvars(tokens(mycorpus) + tokens("And the third sample document.")),
        data.frame()
    )
})