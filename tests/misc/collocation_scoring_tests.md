collocations verification
=========================

Example by Jouni, via Ken
-------------------------

This is follows up on [this thread](https://github.com/quanteda/quanteda/issues/803).

**Jouni**: Please see also the code here to produce a unit test from line 50 onward of [`tests/testthat/test-textstat_collocations.R`](https://github.com/quanteda/quanteda/blob/collocations_verify/tests/testthat/test-textstat_collocations.R#L50-L159) from the `collocations_verify` branch. We want to make sure our tests are correct, and then make sure our code output matches the correct tests.

Two models are fitted here: the one with all two-way interactions ((W1W2, W1W3, W2W3) in the log-linear models language) and the saturated model (W1W2W3). The models are fitted in two ways, which are equivalent: (1) as a Poisson log-linear model for the counts, (2) as a binomial logistic model for the third word given the other two; note here R wants the counts of both the "successes" (W3 is "tax") and "failures" (W3 is other) as part of the definition of the response variable.

Here the likelihood ratio test between these two models is 0.25604 (with 1df), the three-way interaction parameter (lambda) is -1.072 (with se= 2.164 and z-statistic of -0.496, the square of which is 0.246, close to the LR test statistic). You can see in the output how these numbers appear under both ways of fitting the model.

As discussed before, it is not really necessary to fit the saturated model to get lambda and its standard error, as these are simple functions of the counts. The script also shows this calculation to get the -1.072 and the 2.164.

Here the interaction parameter is not significant (although we cannot really conclude this, because the test assumes independence between different triplets) and its estimate is negative. Here seeing capital and gains together thus actually increases the probability seeing "tax" to a lesser extent than would be expected from the sum of the individual effects of capital and gains. In sum, there is no evidence that "capital gains tax" is a true 3-gram in the text where these counts came from.

``` r
data.tmp <- data.frame(word1 = c("capital","other","capital","other", 
                               "capital","other","capital","other"),  
                       word2 = c("gains","gains","other","other", 
                               "gains","gains","other","other"),
                       word3 = c("other","other","other","other", 
                              "tax","tax","tax","tax"),
                       n = c(1.5, 3.5, 2.5, 12.5, 5.5, 2.5, 1.5, 0.5))

data.tmp
```

    ##     word1 word2 word3    n
    ## 1 capital gains other  1.5
    ## 2   other gains other  3.5
    ## 3 capital other other  2.5
    ## 4   other other other 12.5
    ## 5 capital gains   tax  5.5
    ## 6   other gains   tax  2.5
    ## 7 capital other   tax  1.5
    ## 8   other other   tax  0.5

``` r
# For convenience of interpreting the output below, make "other" the reference level
data.tmp$word1 <- relevel(data.tmp$word1, "other")
data.tmp$word2 <- relevel(data.tmp$word2, "other")
data.tmp$word3 <- relevel(data.tmp$word3, "other")

# Models fitted as Poisson log-linear models:  
mP1.tmp <- glm(n ~ word1*word2+word1*word3+word2*word3, family = "poisson", data = data.tmp)
mP2.tmp <- glm(n ~ word1*word2*word3, family="poisson",data=data.tmp)
summary(mP1.tmp)
```

    ## 
    ## Call:
    ## glm(formula = n ~ word1 * word2 + word1 * word3 + word2 * word3, 
    ##     family = "poisson", data = data.tmp)
    ## 
    ## Deviance Residuals: 
    ##        1         2         3         4         5         6         7  
    ##  0.20978  -0.12673  -0.14866   0.06903  -0.10191   0.15861   0.20978  
    ##        8  
    ## -0.29927  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               2.5061     0.2829   8.859  < 2e-16 ***
    ## word1capital             -1.4973     0.6321  -2.369  0.01785 *  
    ## word2gains               -1.1864     0.5659  -2.096  0.03605 *  
    ## word3tax                 -2.8039     0.9749  -2.876  0.00402 ** 
    ## word1capital:word2gains   0.4067     0.9876   0.412  0.68049    
    ## word1capital:word3tax     2.0242     1.0092   2.006  0.04489 *  
    ## word2gains:word3tax       2.2984     1.0329   2.225  0.02607 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 22.26199  on 7  degrees of freedom
    ## Residual deviance:  0.25604  on 1  degrees of freedom
    ## AIC: Inf
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(mP2.tmp)
```

    ## 
    ## Call:
    ## glm(formula = n ~ word1 * word2 * word3, family = "poisson", 
    ##     data = data.tmp)
    ## 
    ## Deviance Residuals: 
    ## [1]  0  0  0  0  0  0  0  0
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                        2.5257     0.2828   8.930   <2e-16 ***
    ## word1capital                      -1.6094     0.6928  -2.323   0.0202 *  
    ## word2gains                        -1.2730     0.6047  -2.105   0.0353 *  
    ## word3tax                          -3.2189     1.4422  -2.232   0.0256 *  
    ## word1capital:word2gains            0.7621     1.1968   0.637   0.5243    
    ## word1capital:word3tax              2.7081     1.7739   1.527   0.1269    
    ## word2gains:word3tax                2.8824     1.6630   1.733   0.0831 .  
    ## word1capital:word2gains:word3tax  -1.0723     2.1635  -0.496   0.6202    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance:  2.2262e+01  on 7  degrees of freedom
    ## Residual deviance: -6.6363e-29  on 0  degrees of freedom
    ## AIC: Inf
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
anova(mP1.tmp, mP2.tmp)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: n ~ word1 * word2 + word1 * word3 + word2 * word3
    ## Model 2: n ~ word1 * word2 * word3
    ##   Resid. Df Resid. Dev Df Deviance
    ## 1         1    0.25604            
    ## 2         0    0.00000  1  0.25604

``` r
# The interaction parameter and its standard error, calculated directly from the counts
(log(data.tmp$n[c(1,3,5,7)]) %*% c(-1, 1, 1, -1)) - (log(data.tmp$n[c(2,4,6,8)]) %*% c(-1, 1, 1, -1))
```

    ##           [,1]
    ## [1,] -1.072295

``` r
sqrt(sum(1/data.tmp$n))
```

    ## [1] 2.163531

``` r
# Models fitted as Binomial logistic models:  
data2.tmp <- data.tmp[1:4, -(3:4)]
data2.tmp$nOther <- data.tmp$n[1:4]
data2.tmp$nTax <- data.tmp$n[5:8]

data2.tmp
```

    ##     word1 word2 nOther nTax
    ## 1 capital gains    1.5  5.5
    ## 2   other gains    3.5  2.5
    ## 3 capital other    2.5  1.5
    ## 4   other other   12.5  0.5

``` r
mB1.tmp <- glm(as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 + word2, family="binomial", data=data2.tmp)
mB2.tmp <- glm(as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 * word2, family="binomial", data=data2.tmp)

summary(mB1.tmp)
```

    ## 
    ## Call:
    ## glm(formula = as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 + 
    ##     word2, family = "binomial", data = data2.tmp)
    ## 
    ## Deviance Residuals: 
    ##       1        2        3        4  
    ## -0.2332   0.2030   0.2571  -0.3071  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)   -2.8039     0.9749  -2.876  0.00402 **
    ## word1capital   2.0242     1.0092   2.006  0.04489 * 
    ## word2gains     2.2984     1.0329   2.225  0.02607 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13.23529  on 3  degrees of freedom
    ## Residual deviance:  0.25604  on 1  degrees of freedom
    ## AIC: 14.272
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(mB2.tmp)
```

    ## 
    ## Call:
    ## glm(formula = as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 * 
    ##     word2, family = "binomial", data = data2.tmp)
    ## 
    ## Deviance Residuals: 
    ## [1]  0  0  0  0
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)               -3.219      1.442  -2.232   0.0256 *
    ## word1capital               2.708      1.774   1.527   0.1269  
    ## word2gains                 2.882      1.663   1.733   0.0831 .
    ## word1capital:word2gains   -1.072      2.164  -0.496   0.6202  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance:  1.3235e+01  on 3  degrees of freedom
    ## Residual deviance: -1.4433e-15  on 0  degrees of freedom
    ## AIC: 15.72
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
anova(mB1.tmp,mB2.tmp)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 + word2
    ## Model 2: as.matrix(data2.tmp[, c("nTax", "nOther")]) ~ word1 * word2
    ##   Resid. Df Resid. Dev Df Deviance
    ## 1         1    0.25604            
    ## 2         0    0.00000  1  0.25604
