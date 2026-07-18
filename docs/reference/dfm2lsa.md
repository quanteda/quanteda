# Convert a dfm to an lsa "textmatrix"

Converts a dfm to a textmatrix for use with the lsa package.

## Usage

``` r
dfm2lsa(x)
```

## Arguments

- x:

  dfm to be converted

## Examples

``` r
if (FALSE) { # \dontrun{
(dfmat <- dfm(tokens(c(d1 = "this is a first matrix",
                       d2 = "this is second matrix as example"))))
lsa::lsa(convert(dfmat, to = "lsa"))
} # }
```
