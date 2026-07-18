# Identify the most frequent features in a dfm

List the most (or least) frequently occurring features in a
[dfm](https://quanteda.io/reference/dfm.md), either as a whole or
separated by document.

## Usage

``` r
topfeatures(
  x,
  n = 10,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = NULL
)
```

## Arguments

- x:

  the object whose features will be returned

- n:

  how many top features should be returned

- decreasing:

  If `TRUE`, return the `n` most frequent features; otherwise return the
  `n` least frequent features

- scheme:

  one of `count` for total feature frequency (within `group` if
  applicable), or `docfreq` for the document frequencies of features

- groups:

  grouping variable for sampling, equal in length to the number of
  documents. This will be evaluated in the docvars data.frame, so that
  docvars may be referred to by name without quoting. This also changes
  previous behaviours for `groups`. See
  `news(Version >= "3.0", package = "quanteda")` for details.

## Value

A named numeric vector of feature counts, where the names are the
feature labels, or a list of these if `groups` is given.

## Examples

``` r
dfmat1 <- corpus_subset(data_corpus_inaugural, Year > 1980) |>
    tokens(remove_punct = TRUE) |>
    dfm()
dfmat2 <- dfm_remove(dfmat1, stopwords("en"))

# most frequent features
topfeatures(dfmat1)
#>  the  and   of   to   we  our    a   in will   is 
#> 1327 1167  929  736  712  679  522  408  360  358 
topfeatures(dfmat2)
#>      us america  nation    must  people   world     new     can     one    time 
#>     194     134     110     110     107     105     102      90      83      80 

# least frequent features
topfeatures(dfmat2, decreasing = FALSE)
#>     hatfield      mondale        baker       moomaw    momentous   occurrence 
#>            1            1            1            1            1            1 
#>    routinely       unique       really every-4-year 
#>            1            1            1            1 

# top features of individual documents
topfeatures(dfmat2, n = 5, groups = docnames(dfmat2))
#> Error in eval(groupsub): object 'dfmat2' not found

# grouping by president last name
topfeatures(dfmat2, n = 5, groups = President)
#> $Biden
#>      us america     can     one  nation 
#>      27      18      16      15      12 
#> 
#> $Bush
#> freedom      us  nation america     can 
#>      36      27      27      27      24 
#> 
#> $Clinton
#>      us     new   world    must america 
#>      40      38      28      28      26 
#> 
#> $Obama
#>     us   must    can nation people 
#>     44     25     20     18     18 
#> 
#> $Reagan
#>         us government     people      world        one 
#>         52         29         25         23         22 
#> 
#> $Trump
#>  america    thank   nation  country american 
#>       36       26       25       24       24 
#> 

# features by document frequencies
tail(topfeatures(dfmat1, scheme = "docfreq", n = 200))
#> greatest  believe     seek     made     born     back 
#>        8        8        8        8        8        8 
```
