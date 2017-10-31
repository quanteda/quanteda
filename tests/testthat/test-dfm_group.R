context("test dfm_select")


test_that("test dfm_group", {
    
    testdfm <- dfm(c('a b c c', 'b c d', 'a'))
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c('doc1', 'doc1', 'doc2'))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c(1, 1, 2))),
        matrix(c(1, 1, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
})

test_that("dfm_group works with empty documents", {
    
    testdfm <- dfm(c('a b c c', 'b c d', ''))
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c('doc1', 'doc1', 'doc2'))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
    
    expect_equivalent(
        as.matrix(dfm_group(testdfm, c(1, 1, 2))),
        matrix(c(1, 0, 2, 0, 3, 0, 1, 0), nrow = 2, 
               dimnames = list(c('doc1', 'doc2'), c('a', 'b', 'c', 'd')))
    )
})

test_that("dfm_group works with docvars", {
    mycorpus <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
                       docvars = data.frame(grp = c(1, 1, 2, 2)))
    mydfm <- dfm(mycorpus)
    expect_equal(
        colSums(dfm_group(mydfm, groups = "grp")),
        colSums(mydfm)
    )
})

test_that("dfm.character groups works (#794)", {
    txt <- c(d1 = "one two three", d2 = "two three four", d3 = "one three four")
    corp <- corpus(txt, docvars = data.frame(grp = c(1, 1, 2)))
    toks <- tokens(corp)
    expect_equal(
        dfm(txt, groups = docvars(corp, "grp")),
        dfm(toks, groups = "grp")
    )
    expect_equal(
        dfm(txt, groups = docvars(corp, "grp")),
        dfm(corp, groups = "grp")
    )
})

test_that("test dfm_group with factor levels, fill = TRUE and FALSE, #854", {
    
    corp <- corpus(c('a b c c', 'b c d', 'a'),
                   docvars = data.frame(grp = factor(c("A", "A", "B"), levels = LETTERS[1:4])))
    testdfm <- dfm(corp)
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = "grp", fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2, 
               dimnames = list(docs = c("A", "B"), features = letters[1:4]))
    )
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = "grp", fill = TRUE)),
        matrix(c(1,2,3,1, 1,0,0,0, 0,0,0,0, 0,0,0,0), byrow = TRUE, nrow = 4 , 
               dimnames = list(docs = c("A", "B", "C", "D"), features = letters[1:4]))
    )
    
    testdfm <- dfm(c('a b c c', 'b c d', 'a'))
    external_factor <- factor(c("text1", "text1", "text2"), 
                              levels = paste0("text", 0:3))
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = external_factor, fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2, 
               dimnames = list(docs = c('text1', 'text2'), features = letters[1:4]))
    )
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = external_factor, fill = TRUE)),
        matrix(c(0,0,0,0, 1,2,3,1, 1,0,0,0, 0,0,0,0), byrow = TRUE, nrow = 4, 
               dimnames = list(docs = paste0("text", 0:3), features = letters[1:4]))
    )
    # new documents in factor order
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = factor(c(1, 1, 2), levels = 4:1), fill = TRUE)),
        matrix(c(rep(0, 8), 1,0,0,0, 1,2,3,1), byrow = TRUE, nrow = 4,
               dimnames = list(docs = 4:1, features = letters[1:4]))
    )
    # should this also be ordered? (here the expectation is that it is)
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = factor(c(3, 3, 1), levels = 4:1), fill = FALSE)),
        matrix(c(1,2,3,1, 1,0,0,0), byrow = TRUE, nrow = 2,
               dimnames = list(docs = c(3, 1), features = letters[1:4]))
    )
})

test_that("test dfm_group with non-factor grouping variable, with fill", {
    grp <- c("D", "D", "A", "C")
    corp <- corpus(c('a b c c', 'b c d', 'a', "b d d"),
                   docvars = data.frame(grp = grp, stringsAsFactors = FALSE))
    testdfm <- dfm(corp)
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = "grp", fill = FALSE)),
        matrix(c(1,0,0,0, 0,1,0,2, 1,2,3,1), byrow = TRUE, nrow = 3, 
               dimnames = list(docs = c("A", "C", "D"), features = letters[1:4]))
    )
    expect_equal(
        dfm_group(testdfm, groups = "grp", fill = FALSE),
        dfm_group(testdfm, groups = "grp", fill = TRUE)
    )
    expect_equal(
        dfm_group(testdfm, groups = grp, fill = FALSE),
        dfm_group(testdfm, groups = grp, fill = TRUE)
    )
    
    expect_equal(
        as.matrix(dfm_group(testdfm, groups = grp, fill = FALSE)),
        matrix(c(1,0,0,0, 0,1,0,2, 1,2,3,1), byrow = TRUE, nrow = 3, 
               dimnames = list(docs = c("A", "C", "D"), features = letters[1:4]))
    )
    expect_equal(
        dfm_group(testdfm, groups = grp, fill = FALSE),
        dfm_group(testdfm, groups = "grp", fill = FALSE)
    )
})
    
test_that("test dfm_group with wrongly dimensioned groups variables", {
    grp <- c("D", "D", "A", "C")
    corp <- corpus(c('a b c c', 'b c d', 'a', "b d d"),
                   docvars = data.frame(grp = grp, stringsAsFactors = FALSE))
    testdfm <- dfm(corp)
    expect_error(
        dfm_group(testdfm, groups = c(1, 1, 2, 3, 3), fill = FALSE),
        "groups must name docvars or provide data matching the documents in x"
    )
    expect_error(
        dfm_group(testdfm, groups = c(1, 1, 2, 3, 3), fill = TRUE),
        "groups must name docvars or provide data matching the documents in x"
    )
    expect_error(
        dfm_group(testdfm, groups = c(1, 1, 2, 3, 4), fill = TRUE),
        "groups must name docvars or provide data matching the documents in x"
    )
})
