require(quanteda)
require(testthat)
context('Testing fcm*.R')

test_that("compare the output feature co-occurrence matrix to that of the text2vec package", {
    skip_if_not_installed("text2vec")
    require(text2vec)
    
    txt <- "A D A C E A D F E B A C E D"
    tokens <- txt %>% tolower %>% word_tokenizer
    it <- itoken(tokens)
    v <- create_vocabulary(it)
    vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
    tcm <- create_tcm(itoken(tokens), vectorizer)
    
    # convert to a symmetric matrix to facilitate the sorting
    tcm <- as.matrix(tcm)
    tcm <- tcm + t(tcm)
    
    # sort the matrix according to rowname-colname and convert back to a upper triangle matrix
    tcm <- tcm[order(rownames(tcm)), order(colnames(tcm))]
    tcm[lower.tri(tcm,diag = FALSE)] <- 0
    
    toks <- tokenize(char_tolower(txt), removePunct = TRUE)
    fcm <- fcm(toks, context = "window", count = "weighted", window = 3)
    fcm <- fcm_sort(fcm)
    diag(fcm) <- 0
    expect_true(all(round(fcm, 2) == round(tcm, 2)))
    
})

# Testing weighting function
txt <- "A D A C E A D F E B A C E D"

test_that("not weighted",{
    fcm <- fcm(txt, context = "window", window = 3)           
    aMat <- matrix(c(2, 1, 4, 4, 5, 2,
                     0, 0, 1, 1, 2, 1,
                     0, 0, 0, 3, 3, 0,
                     0, 0, 0, 0, 4, 1,
                     0, 0, 0, 0, 0, 2,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    fcm <- fcm_sort(fcm)
    expect_equivalent(as.matrix(fcm), aMat)
})

test_that("weighted by default",{
    fcm <- fcm(txt, context = "window", count = "weighted", window = 3)           
    fcm <- fcm_sort(fcm)
    
    # tokenizedTexts
    toks <- tokenize(txt)
    fcmTexts <- fcm(toks, context = "window", count = "weighted", window = 3) 
    
    aMat <- matrix(c(0.83, 1, 2.83, 3.33, 2.83, 0.83,
                     0, 0, 0.5, 0.33, 1.33, 0.50,
                     0, 0, 0, 1.33, 2.33, 0,
                     0, 0, 0, 0, 2.33, 1.00,
                     0, 0, 0, 0, 0, 1.33,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(aMat, round(as.matrix(fcm), 2), round(as.matrix(fcmTexts), 2))
})

test_that("customized weighting function",{
    fcm <- fcm(txt, context = "window", count = "weighted", weights = c(3,2,1), window = 3)           
    fcm <- fcm_sort(fcm)
    
    # tokenizedTexts
    toks <- tokenize(txt)
    fcmTexts <- fcm(toks, context = "window", count = "weighted", window = 3) 
    fcmTexts <- fcm_sort(fcmTexts)
    
    aMat <- matrix(c(3, 3, 9, 10, 10, 3,
                     0, 0, 2, 1, 4, 2,
                     0, 0, 0, 5, 7, 0,
                     0, 0, 0, 0, 8, 3,
                     0, 0, 0, 0, 0, 4,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(aMat, round(as.matrix(fcm), 2), round(as.matrix(fcmTexts), 2))
})

# Testing 'ordered' 
txt <- "A D A C E A D F E B A C E D"
test_that("ordered setting: window",{
    fcm <- fcm(txt, context = "window", window = 3, ordered = TRUE, tri = FALSE)           
    fcm <- fcm_sort(fcm)
    
    aMat <- matrix(c( 2, 0, 3, 3, 3, 1,
                      1, 0, 1, 0, 1, 0,
                      1, 0, 0, 2, 2, 0,
                      1, 1, 1, 0, 2, 1,
                      2, 1, 1, 2, 0, 1,
                      1, 1, 0, 0, 1, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcm, 2) == round(aMat, 2)))
    
    # Not ordered
    fcm_nOrd <- fcm(txt, context = "window", window = 3, ordered = FALSE, tri = FALSE) 
    fcm_nOrd <- fcm_sort(fcm_nOrd)
    aMat <- matrix(c(4, 1, 4, 4, 5, 2,
                     1, 0, 1, 1, 2, 1,
                     4, 1, 0, 3, 3, 0,
                     4, 1, 3, 0, 4, 1,
                     5, 2, 3, 4, 0, 2,
                     2, 1, 0, 1, 2, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcm_nOrd, 2) == round(aMat, 2)))
})

test_that("ordered setting: boolean",{
    txts <- c("b a b c", "a a c b e", "a c e f g")
    fcm <- fcm(txts, context = "window", count = "boolean", window = 2, ordered = TRUE, tri = TRUE)           
    fcm <- fcm_sort(fcm)
    
    aMat <- matrix(c(1, 1, 3, 1, 0, 0,
                     0, 1, 1, 1, 0, 0,
                     0, 0, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(aMat, as.matrix(fcm))
    
    # not ordered
    fcm <- fcm(txts, context = "window", count = "boolean", window = 2, ordered = FALSE, tri = TRUE)           
    fcm <- fcm_sort(fcm)
    
    aMat <- matrix(c(1, 2, 3, 1, 0, 0,
                     0, 1, 2, 1, 0, 0,
                     0, 0, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(aMat, as.matrix(fcm))
})
# Testing "count" with multiple documents
txts <- c("a a a b b c", "a a c e", "a c e f g")
test_that("counting the frequency of the co-occurrences",{
    fcm <- fcm(txts, context = "document", count = "frequency", tri = TRUE)           
    fcm <- fcm_sort(fcm)
    aMat <- matrix(c(4, 6, 6, 3, 1, 1,
                     0, 1, 2, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcm, 2) == round(aMat, 2)))
})

test_that("counting the co-occurrences in 'boolean' way",{
    fcm <- fcm(txts, context = "document", count = "boolean")           
    fcm <- fcm_sort(fcm)
    
    aMat <- matrix(c(2, 1, 3, 2, 1, 1,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcm, 2) == round(aMat, 2)))
})

# Testing the setting of window size
txts <- c("a a a b b c", "a a c e", "a c e f g")
test_that("window = 2",{
    fcm <- fcm(txts, context = "window", count = "boolean", window = 2)           
    fcm <- fcm_sort(fcm)
    
    # tokenizedTexts
    toks <- tokenize(txts)
    fcmTexts <- fcm(toks, context = "window", count = "boolean", window = 2) 
    
    aMat <- matrix(c(2, 1, 2, 2, 0, 0,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_equivalent(aMat, as.matrix(fcm), as.matrix(fcmTexts))
})

test_that("window = 3",{
    fcm <- fcm(txts, context = "window", count = "boolean", window = 3)           
    fcm <- fcm_sort(fcm)
    aMat <- matrix(c(2, 1, 3, 2, 1, 0,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(fcm, 2) == round(aMat, 2)))
})

test_that("fcm.dfm works same as fcm.tokens", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), removePunct = TRUE)
    expect_equal(fcm(toks, context = "document"),
                 fcm(dfm(toks), context = "document"))
})

test_that("fcm.dfm only works for context = \"document\"", {
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokens(char_tolower(txt), removePunct = TRUE)
    expect_error(fcm(dfm(toks), context = "window"),
                 "fcm.dfm only works on context = \"document\"")
})

test_that("fcm works as expected for tokens_hashed", {
    
    txt <- c("The quick brown fox jumped over the lazy dog.",
             "The dog jumped and ate the fox.")
    toks <- tokenize(char_tolower(txt), removePunct = TRUE)
    toksh <- tokens(char_tolower(txt), removePunct = TRUE)
    classic <- fcm(toks, context = "window", window = 3)
    hashed <- fcm(toksh, context = "window", window = 3)
    expect_equivalent(classic, hashed)
})

test_that("fcm print works as expected", {
    txts <- c("a a a b b c", "a a c e", "a c e f g")
    testfcm <- fcm(txts, context = "document", count = "frequency", tri = TRUE) 
    expect_output(print(testfcm),
                  "^Feature co-occurrence matrix of: 6 by 6 features.")
    expect_output(print(testfcm[1:5, 1:5]),
                  "^Feature co-occurrence matrix of: 5 by 5 features.")
})

test_that("fcm works the same for different object types", {
    txts <- c("a a a b b c", "a a c e", "a c e f g")
    expect_identical(fcm(txts), fcm(corpus(txts)))
    expect_identical(fcm(tokens(txts)), fcm(corpus(txts)))
    expect_identical(fcm(txts), fcm(tokens(txts)))
})

