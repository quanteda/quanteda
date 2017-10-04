context('Testing textmodel-lsa.R')

test_that("textmodel-lsa (rsvd) works as expected as lsa", {
    skip_if_not_installed("lsa")
    
    foxmatrix <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,1)
    dim(foxmatrix) <- c(3, 4)
    rownames(foxmatrix) <- paste0("D", seq(1:3))
    #{lsa}
    foxlsaMatrix <- as.textmatrix(t(foxmatrix))
    #myMatrix <- lw_logtf(foxlsaMatrix) * gw_idf(foxlsaMatrix)
    
    myLSAspace <- lsa(foxlsaMatrix, dims = 2)
    
    #quanteda
    foxdfm <- as.dfm(foxmatrix)
    qtd_lsa <- textmodel_lsa(foxdfm, nd = 2)
    
    expect_equivalent(abs(qtd_lsa$dk), abs(myLSAspace$dk))
    expect_equivalent(abs(qtd_lsa$tk), abs(myLSAspace$tk))
    expect_equivalent(abs(qtd_lsa$sk), abs(myLSAspace$sk))
})

test_that("transform-lsa works as expected as lsa", {
    skip_if_not_installed("lsa")
    
    foxmatrix <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1)
    dim(foxmatrix) <- c(3, 4)
    rownames(foxmatrix) <- paste0("D", seq(1:3))
    
    newfox <- matrix(c(1, 0, 1, 0, 1, 1, 0, 0), nrow = 2, ncol = 4, byrow = TRUE)
    rownames(newfox) <- paste0("D", c(4:5))
    #{lsa}
    foxlsaMatrix <- as.textmatrix(t(foxmatrix))
    myLSAspace <- lsa(foxlsaMatrix, dims = 2)
    newSpace <- t(fold_in(t(newfox), myLSAspace))
    newSpace <- newSpace[ , ]
    #quanteda
    foxdfm <- as.dfm(foxmatrix)
    qtd_lsa <- textmodel_lsa(foxdfm, nd = 2)
    new_qtd_lsa <- transform_lsa(newfox, qtd_lsa)
    
    expect_equivalent(round(abs(new_qtd_lsa), digits = 3), round(abs(newSpace), digits = 3))

})