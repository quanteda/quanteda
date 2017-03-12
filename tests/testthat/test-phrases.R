context('test phrases.R')

test_that("as.corpus.corpuszip works: text vs dictionary", {
    mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
                 "New York City has raised a taxes: an income tax and a sales tax.")
    mydict <- dictionary(list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax")))
    cw <- phrasetotoken(mytexts, mydict)
    expect_output(print(cw), "capital_gains_tax")
    expect_failure(expect_output(print(cw), "capital gains tax"))
    
    expect_output(print(cw), "income_tax")
    expect_failure(expect_output(print(cw), "income tax"))
    
    expect_output(print(cw), "inheritance_tax")
    expect_failure(expect_output(print(cw), "inheritance tax"))
    
    expect_output(print(cw), "tax")
})

test_that("as.corpus.corpuszip works: tokenizedTexts", {
    toks <- tokenize(c("The new law included a capital gains tax, and an inheritance tax.",
                 "New York City has raised a taxes: an income tax and a sales tax."))
    myphrases <- c("tax", "income tax", "capital gains tax", "inheritance tax")
    cw <- phrasetotoken(toks, myphrases)
    expect_output(print(cw), "capital_gains_tax")
    expect_failure(expect_output(print(cw), "capital gains tax"))
    
    expect_output(print(cw), "income_tax")
    expect_failure(expect_output(print(cw), "income tax"))
    
    expect_output(print(cw), "inheritance_tax")
    expect_failure(expect_output(print(cw), "inheritance tax"))
    
    expect_output(print(cw), "tax")
})

test_that("as.corpus.corpuszip works: text vs collocations", {
    mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
                 "New York City has raised a taxes: an income tax and a sales tax.")
    mycoll <- collocations(c("tax", "income tax", "capital gains tax", "inheritance tax"), size = 2:3)
    cw <- phrasetotoken(mytexts, mycoll)
    expect_output(print(cw), "capital_gains_tax")
    expect_failure(expect_output(print(cw), "capital gains tax"))
    
    expect_output(print(cw), "income_tax")
    expect_failure(expect_output(print(cw), "income tax"))
    
    expect_output(print(cw), "inheritance_tax")
    expect_failure(expect_output(print(cw), "inheritance tax"))
    
    expect_output(print(cw), "tax")
})
