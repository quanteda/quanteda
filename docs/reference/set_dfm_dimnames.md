# Internal functions to set dimnames

Default [`dimnames()`](https://rdrr.io/r/base/dimnames.html) converts a
zero-length character vector to NULL, leading to the improper
functioning of subsetting functions. These are safer methods to set the
dimnames of a dfm or fcm object.

## Usage

``` r
set_dfm_dimnames(x) <- value

set_dfm_docnames(x) <- value

set_dfm_featnames(x) <- value

set_fcm_dimnames(x) <- value

set_fcm_featnames(x) <- value
```

## Arguments

- x:

  [dfm](https://quanteda.io/reference/dfm.md) or
  [fcm](https://quanteda.io/reference/fcm.md)

- value:

  character a vector for docnames or featnames or a list of them for
  dimnames

## Examples

``` r
dfmat <- dfm(tokens(c("a a b b c", "b b b c")))
quanteda:::set_dfm_featnames(dfmat) <- paste0("feature", 1:3)
quanteda:::set_dfm_docnames(dfmat) <- paste0("DOC", 1:2)
quanteda:::set_dfm_dimnames(dfmat) <- list(c("docA", "docB"), LETTERS[1:3])
```
