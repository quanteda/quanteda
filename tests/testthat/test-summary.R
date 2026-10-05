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
             ntoken = 154888,
             ntype = 10332,
             ndocvar = 4
        )
    )
    
    dfmt <- dfm(toks)
    expect_output(
        stat_dfmt <- summary(dfmt),
        "dfm of 60 documents x 9,591 features (154,888 occurrences, 91.94% sparsity) and 4 docvars.", 
        fixed = TRUE
    )
    expect_equal(
        stat_dfmt,
        list(ndoc = 60,
             nocc = 154888,
             nfeat = 9591,
             spar = 0.9193,
             ndocvar = 4,
             tolerance = 0.01
        )
    )
    
    fcmt <- fcm(dfmt)
    expect_output(
        stat_fcmt <- summary(fcmt),
        "fcm of 634 x 634 features (1,194,222 co-occurrences, 54.33% sparsity).", 
        fixed = TRUE
    )
    expect_equal(
        stat_fcmt,
        list(nrow = 634,
             ncol = 634,
             nocc = 154888,
             nfeat = 9591,
             spar = 0.5432,
             tolerance = 0.01
        )
    )
    
})
