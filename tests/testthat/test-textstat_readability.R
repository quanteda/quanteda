context("textstat_readability")

test_that("readability works: basic", {
    txt <- "This was adjusted by a prolongation of the period of reimbursement in nature of a new loan 
            at an interest of 5% for the term of ten years, and the expenses of this operation were a commission of 3%.  
            The first installment of the loan of $2,000,000 from the Bank of the United States has been paid, as was directed by law.  
            For the second it is necessary that provision be made.  
            No pecuniary consideration is more urgent than the regular redemption and discharge of the public debt."
    expect_true(!is.na(textstat_readability(txt, "Flesch")))
})

test_that("readability count is ok", {
    expect_equal(round(textstat_readability("The cat in the hat ate breakfast.", "Flesch")), c(text1 = 103))
    expect_equal(textstat_readability("The cat in the hat ate breakfast.", "FOG"), c(text1 = 2.8))
})
    