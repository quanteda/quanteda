# Match the dfm columns with given features

Match the columns of multiple
[dfm](https://quanteda.io/reference/dfm.md) objects using a character
vector.

## Usage

``` r
dfm_match(x, features, verbose = quanteda_options("verbose"))
```

## Arguments

- x:

  the [dfm](https://quanteda.io/reference/dfm.md) object.

- features:

  character vector for the feature names to be matched in the resulting
  [dfm](https://quanteda.io/reference/dfm.md). Columns not included in
  `features` are removed.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

A [dfm](https://quanteda.io/reference/dfm.md) whose features are
identical to those specified in `features`.

## Details

Matching the dfm columns is necessary when you split a corpus into two:
you fit a model on the test set and evaluation it on the test set whose
features must be identical. It is also used in
[`bootstrap_dfm()`](https://quanteda.io/reference/bootstrap_dfm.md).

## Note

Unlike [`dfm_select()`](https://quanteda.io/reference/dfm_select.md),
this function will add feature names not already present in `x`. It also
provides only fixed, case-sensitive matches. For more flexible feature
selection, see
[`dfm_select()`](https://quanteda.io/reference/dfm_select.md).

## See also

[`dfm_select()`](https://quanteda.io/reference/dfm_select.md)

## Examples

``` r
# matching a dfm to a feature vector
dfm_match(dfm(tokens("")), letters[1:5])
#> Document-feature matrix of: 1 document, 5 features (100.00% sparse) and 0
#> docvars.
#>        features
#> docs    a b c d e
#>   text1 0 0 0 0 0
dfm_match(data_dfm_lbgexample, c("A", "B", "Z"))
#> Document-feature matrix of: 6 documents, 3 features (72.22% sparse) and 0
#> docvars.
#>     features
#> docs A B   Z
#>   R1 2 3   0
#>   R2 0 0   0
#>   R3 0 0   3
#>   R4 0 0 115
#>   R5 0 0  78
#>   V1 0 0   0
dfm_match(data_dfm_lbgexample, c("B", "newfeat1", "A", "newfeat2"))
#> Document-feature matrix of: 6 documents, 4 features (91.67% sparse) and 0
#> docvars.
#>     features
#> docs B newfeat1 A newfeat2
#>   R1 3        0 2        0
#>   R2 0        0 0        0
#>   R3 0        0 0        0
#>   R4 0        0 0        0
#>   R5 0        0 0        0
#>   V1 0        0 0        0

# matching one dfm to another
txt <- c("This is text one", "The text two", "This is text three")
(dfmt1 <- dfm(tokens(txt[1:2])))
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 0
#> docvars.
#>        features
#> docs    this is text one the two
#>   text1    1  1    1   1   0   0
#>   text2    0  0    1   0   1   1
(dfmt2 <- dfm(tokens(txt[2:3])))
#> Document-feature matrix of: 2 documents, 6 features (41.67% sparse) and 0
#> docvars.
#>        features
#> docs    the text two this is three
#>   text1   1    1   1    0  0     0
#>   text2   0    1   0    1  1     1
(dfmt3 <- dfm(dfm_match(dfmt1, featnames(dfmt2))))
#> Document-feature matrix of: 2 documents, 5 features (40.00% sparse) and 0
#> docvars.
#>        features
#> docs    the text two this is
#>   text1   0    1   0    1  1
#>   text2   1    1   1    0  0
identical(featnames(dfmt2), featnames(dfmt3))
#> [1] FALSE
```
