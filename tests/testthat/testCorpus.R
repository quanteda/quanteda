require(quanteda)

context('Testing corpus.R')

test_that("docvars of corpus is a data.frame", {
    expect_that(
        docvars(inaugCorpus),
        is_a('data.frame')
    )
})

test_that("test corpus.c", {
    concat.corpus <- c(inaugCorpus, inaugCorpus, inaugCorpus)

    expect_that(
        docvars(concat.corpus),
        equals(
           rbind(docvars(inaugCorpus), docvars(inaugCorpus), docvars(inaugCorpus))
           )
    )

    expect_that(
        docvars(concat.corpus),
        is_a('data.frame')
    )

    expect_that(
        texts(concat.corpus),
        equals(
           c(texts(inaugCorpus), texts(inaugCorpus), texts(inaugCorpus))
           )
    )
    expect_that(
        texts(concat.corpus),
        is_a('data.frame')
    )


    expect_true(
        grepl('Concatenation by c.corpus', metacorpus(concat.corpus)$source)
    )

    expect_that(
        texts(concat.corpus),
        equals(
           c(texts(inaugCorpus), texts(inaugCorpus))
           )
    )

})


