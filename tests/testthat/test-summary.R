test_that("summary works", {
    
    corp <- data_corpus_inaugural
    expect_output(

        stat_corp <- summary(corp),
        "corpus of 60 documents (823,688 characters) and 4 docvars.", 
        fixed = TRUE
    )
    expect_equal(
        stat_corp,
        list(ndoc = 60,
             nchar = 823688,
             ndocvar = 4
        )
    )

    toks <- tokens(data_corpus_inaugural)
    expect_output(
        stat_toks <- summary(toks),
        "tokens of 60 documents (154,888 tokens, 10,332 types) and 4 docvars.", 
        fixed = TRUE
    )
    expect_equal(
        stat_toks,
        list(ndoc = 60,
             ntype = 10332,
             ntoken = 154888,
             ndocvar = 4
        )
    )
    
    dfmt <- dfm(toks)
    expect_output(
        stat_dfmt <- summary(dfmt),
        "dfm of 60 documents x 9,591 features (154,888 occurrences, 91.94% sparsity) and\n4 docvars.", 
        fixed = TRUE
    )
    expect_equal(
        stat_dfmt,
        list(ndoc = 60,
             nfeat = 9591,
             nocc = 154888,
             spar = 0.9193,
             ndocvar = 4
        ), tolerance = 0.01
    )
    
    fcmt <- fcm(dfmt)
    expect_output(
        stat_fcmt <- summary(fcmt),
        "fcm of 9,591 x 9,591 features (263,431,760 co-occurrences, 88.58% sparsity).", 
        fixed = TRUE
    )
    expect_equal(
        stat_fcmt,
        list(nrow = 9591,
             ncol = 9591,
             nocc = 263431760,
             spar = 0.8858
        ), tolerance = 0.01
    )
    
})
