context('Testing cfm*.R')

test_that("compare the output context-feature matrix to that of the text2vec package", {
    library(text2vec)
    txt <- "A D A C E A D F E B A C E D"
    #txt <- c("The quick brown fox jumped over the lazy dog.",
    #          "The dog jumped and ate the fox.")
    #txt <- subset(inaugCorpus, Year > 1900)
    #txt <- subset(inaugCorpus, Year > 1900 & Year<1902)
    # text2vec considers txt$metadata, txt$setting as part of the document
    # text2vec doesn't remove '_' , but does remove '-'
    # text2vec treats newline character "\n" as part of the word
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
    
    toks <- tokenize(toLower(txt), removePunct = TRUE)
    cfm <- cfm(toks, context = "window", count = "weighted", window = 3)
    diag(cfm) <- 0
    expect_true(all(round(cfm, 2) == round(tcm, 2)))
    
})

# Testing weighting function
txt <- "A D A C E A D F E B A C E D"

test_that("not weighted",{
    cfm <- cfm(txt, context = "window", window = 3)           
    aMat <- matrix(c(2, 1, 4, 4, 5, 2,
                     0, 0, 1, 1, 2, 1,
                     0, 0, 0, 3, 3, 0,
                     0, 0, 0, 0, 4, 1,
                     0, 0, 0, 0, 0, 2,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

test_that("weighted by default",{
    cfm <- cfm(txt, context = "window", count = "weighted", window = 3)           
    aMat <- matrix(c(0.83, 1, 2.83, 3.33, 2.83, 0.83,
                     0, 0, 0.5, 0.33, 1.33, 0.50,
                     0, 0, 0, 1.33, 2.33, 0,
                     0, 0, 0, 0, 2.33, 1.00,
                     0, 0, 0, 0, 0, 1.33,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

test_that("customized weighting function",{
    cfm <- cfm(txt, context = "window", count = "weighted", weights = c(3,2,1), window = 3)           
    aMat <- matrix(c(3, 3, 9, 10, 10, 3,
                     0, 0, 2, 1, 4, 2,
                     0, 0, 0, 5, 7, 0,
                     0, 0, 0, 0, 8, 3,
                     0, 0, 0, 0, 0, 4,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

# Testing 'ordered' 
txt <- "A D A C E A D F E B A C E D"
test_that("customized weighting function",{
    cfm <- cfm(txt, context = "window", count = "weighted", weights = c(3,2,1), window = 3, ordered = TRUE, tri = FALSE)           
    aMat <- matrix(c(3, 0, 7, 7, 5, 2,
                     3, 0, 2, 0, 1, 0,
                     2, 0, 0, 3, 6, 0,
                     3, 1, 2, 0, 3, 3,
                     5, 3, 1, 5, 0, 1,
                     1, 2, 0, 0, 3, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

# Testing "count" with multiple documents
txts <- c("a a a b b c", "a a c e", "a c e f g")
test_that("counting the frequency of the co-occurrences",{
    cfm <- cfm(txts, context = "document", count = "frequency")           
    aMat <- matrix(c(4, 6, 6, 3, 1, 1,
                     0, 1, 2, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

test_that("counting the co-occurrences in 'boolean' way",{
    cfm <- cfm(txts, context = "document", count = "boolean")           
    aMat <- matrix(c(2, 1, 3, 2, 1, 1,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

# Testing the setting of window size
txts <- c("a a a b b c", "a a c e", "a c e f g")
test_that("window = 2",{
    cfm <- cfm(txts, context = "window", count = "boolean", window = 2)           
    aMat <- matrix(c(2, 1, 2, 2, 0, 0,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 0,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})

test_that("window = 3",{
    cfm <- cfm(txts, context = "window", count = "boolean", window = 3)           
    aMat <- matrix(c(2, 1, 3, 2, 1, 0,
                     0, 1, 1, 0, 0, 0,
                     0, 0, 0, 2, 1, 1,
                     0, 0, 0, 0, 1, 1,
                     0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0),
                   nrow = 6, ncol = 6, byrow = TRUE)
    expect_true(all(round(cfm, 2) == round(aMat, 2)))
})


