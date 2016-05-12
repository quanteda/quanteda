require(quanteda)

context('Testing corpus.R')

test_that("docvars of corpus is a data.frame", {
    expect_that(
        docvars(inaugCorpus),
        is_a('data.frame')
    )
})

test_that("test c.corpus", {
    concat.corpus <- c(inaugCorpus, inaugCorpus, inaugCorpus)

    expected_docvars <-rbind(docvars(inaugCorpus), docvars(inaugCorpus), docvars(inaugCorpus))
    rownames(expected_docvars) <- make.unique(rep(rownames(docvars(inaugCorpus)), 3), sep='')

    expect_that(
        docvars(concat.corpus),
        equals(expected_docvars)
    )

    expect_that(
        docvars(concat.corpus),
        is_a('data.frame')
    )

    
    expected_texts <- c(texts(inaugCorpus), texts(inaugCorpus), texts(inaugCorpus))
    names(expected_texts) <- make.unique(rep(names(texts(inaugCorpus)), 3), sep='')
  
    expect_that(
        texts(concat.corpus),
        equals(expected_texts)
    )

    expect_that(
        texts(concat.corpus),
        is_a('character')
    )


    expect_true(
        grepl('Concatenation by c.corpus', metacorpus(concat.corpus)$source)
    )

})


