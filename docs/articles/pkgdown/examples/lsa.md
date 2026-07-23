# Example: Latent Semantic Analysis (LSA)

In this vignette, we show how to perform Latent Semantic Analysis using
the **quanteda** package based on Grossman and Frieder’s [*Information
Retrieval, Algorithms and
Heuristics*](http://www1.se.cuhk.edu.hk/%7Eseem5680/lecture/LSI-Eg.pdf).
We can reduce the dimensions of document-feature matrix while keeping
the essential semantic structure.

``` r

library("quanteda")
```

## Create a document-feature matrix

``` r

txt <- c(d1 = "Shipment of gold damaged in a fire",
         d2 = "Delivery of silver arrived in a silver truck",
         d3 = "Shipment of gold arrived in a truck" )

dfmat <- txt |> 
    tokens() |> 
    dfm()
dfmat
## Document-feature matrix of: 3 documents, 11 features (36.36% sparse) and 0
## docvars.
##     features
## docs shipment of gold damaged in a fire delivery silver arrived
##   d1        1  1    1       1  1 1    1        0      0       0
##   d2        0  1    0       0  1 1    0        1      2       1
##   d3        1  1    1       0  1 1    0        0      0       1
## [ reached max_nfeat ... 1 more feature ]
```

## Fit the LSA model

``` r

library("quanteda.textmodels")
tmod_lsa <- textmodel_lsa(dfmat)
## Warning in fun(A, k, nu, nv, opts, mattype = "dgCMatrix"): all singular values
## are requested, svd() is used instead
```

The new document vector coordinates in the reduced 2-dimensional space
is:

``` r

tmod_lsa$docs[, 1:2]
##          [,1]       [,2]
## d1 -0.4944666  0.6491758
## d2 -0.6458224 -0.7194469
## d3 -0.5817355  0.2469149
```

## Apply the model to new data

Now the new unseen document can be represented in the reduced
2-dimensional space.

``` r

dfmat_test <- tokens("gold silver truck") |> 
    dfm() |> 
    dfm_match(features = featnames(dfmat))
dfmat_test
## Document-feature matrix of: 1 document, 11 features (72.73% sparse) and 0
## docvars.
##        features
## docs    shipment of gold damaged in a fire delivery silver arrived
##   text1        0  0    1       0  0 0    0        0      1       0
## [ reached max_nfeat ... 1 more feature ]

pred_lsa <- predict(tmod_lsa, newdata = dfmat_test)
pred_lsa$docs_newspace[, 1:2]
## [1] -0.2140026 -0.1820571
```
