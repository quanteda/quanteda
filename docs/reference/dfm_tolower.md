# Convert the case of the features of a dfm and combine

`dfm_tolower()` and `dfm_toupper()` convert the features of the dfm or
fcm to lower and upper case, respectively, and then recombine the
counts.

## Usage

``` r
dfm_tolower(x, keep_acronyms = FALSE, verbose = quanteda_options("verbose"))

dfm_toupper(x, verbose = quanteda_options("verbose"))

fcm_tolower(x, keep_acronyms = FALSE, verbose = quanteda_options("verbose"))

fcm_toupper(x, verbose = quanteda_options("verbose"))
```

## Arguments

- x:

  the input object whose character/tokens/feature elements will be
  case-converted

- keep_acronyms:

  logical; if `TRUE`, do not lowercase any all-uppercase words (applies
  only to `*_tolower()` functions)

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Details

`fcm_tolower()` and `fcm_toupper()` convert both dimensions of the
[fcm](https://quanteda.io/reference/fcm.md) to lower and upper case,
respectively, and then recombine the counts. This works only on fcm
objects created with `context = "document"`.

## Examples

``` r
# for a document-feature matrix
dfmat <- dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE)
dfmat
#> Document-feature matrix of: 2 documents, 5 features (40.00% sparse) and 0
#> docvars.
#>        features
#> docs    b A C a B
#>   text1 1 2 0 0 0
#>   text2 1 0 2 1 1
dfm_tolower(dfmat)
#> Document-feature matrix of: 2 documents, 3 features (16.67% sparse) and 0
#> docvars.
#>        features
#> docs    b a c
#>   text1 1 2 0
#>   text2 2 1 2
dfm_toupper(dfmat)
#> Document-feature matrix of: 2 documents, 3 features (16.67% sparse) and 0
#> docvars.
#>        features
#> docs    B A C
#>   text1 1 2 0
#>   text2 2 1 2

# for a feature co-occurrence matrix
fcmat <- fcm(tokens(c("b A A d", "C C a b B e")),
             context = "document")
fcmat
#> Feature co-occurrence matrix of: 7 by 7 features.
#>         features
#> features b A d C a B e
#>        b 0 2 1 2 1 1 1
#>        A 0 1 2 0 0 0 0
#>        d 0 0 0 0 0 0 0
#>        C 0 0 0 1 2 2 2
#>        a 0 0 0 0 0 1 1
#>        B 0 0 0 0 0 0 1
#>        e 0 0 0 0 0 0 0
fcm_tolower(fcmat)
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features b a d c e
#>        b 1 3 1 2 2
#>        a 1 1 2 0 1
#>        d 0 0 0 0 0
#>        c 2 2 0 1 2
#>        e 0 0 0 0 0
fcm_toupper(fcmat)
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features B A D C E
#>        B 1 3 1 2 2
#>        A 1 1 2 0 1
#>        D 0 0 0 0 0
#>        C 2 2 0 1 2
#>        E 0 0 0 0 0
```
