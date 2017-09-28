context('Testing textmodel-lsa.R')

test_that("textmodel-lsa (rsvd) works as expected as lsa", {
    foxmatrix <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,1)
    dim(foxmatrix) <- c(3, 4)
    
    #{lsa}
    foxlsaMatrix <- as.textmatrix(foxmatrix)
    myMatrix <- lw_logtf(foxlsaMatrix) * gw_idf(foxlsaMatrix)
    
    myLSAspace <- lsa(myMatrix, dims=dimcalc_share())
    myLSA <- as.textmatrix(myLSAspace)

    
    #quanteda
    foxdfm <- as.dfm(foxmatrix)
    qtd_lsa <- textmodel_lsa(foxdfm)
    
    #text2vec
    tv_tfidf = text2vec::TfIdf$new()
    tv_lsa = text2vec::LSA$new(n_topics = 3, method = "randomized")
    
    doc_embeddings = foxdfm %>% 
        fit_transform(tv_tfidf) %>% 
        fit_transform(tv_lsa)
    
    
})
