# Coercion and checking functions for dfm objects

Convert an eligible input object into a dfm, or check whether an object
is a dfm. Current eligible inputs for coercion to a dfm are:
[matrix](https://rdrr.io/r/base/matrix.html), (sparse)
[Matrix](https://rdrr.io/pkg/Matrix/man/Matrix.html), TermDocumentMatrix
and DocumentTermMatrix (from the tm package),
[data.frame](https://rdrr.io/r/base/data.frame.html), and other
[dfm](https://quanteda.io/reference/dfm.md) objects.

## Usage

``` r
as.dfm(x)

is.dfm(x)
```

## Arguments

- x:

  a candidate object for checking or coercion to
  [dfm](https://quanteda.io/reference/dfm.md)

## Value

`as.dfm` converts an input object into a
[dfm](https://quanteda.io/reference/dfm.md). Row names are used for
docnames, and column names for featnames, of the resulting dfm.

`is.dfm` returns `TRUE` if and only if its argument is a
[dfm](https://quanteda.io/reference/dfm.md).

## See also

[`as.data.frame.dfm()`](https://quanteda.io/reference/as.data.frame.dfm.md),
[`as.matrix.dfm()`](https://quanteda.io/reference/as.matrix.dfm.md),
[`convert()`](https://quanteda.io/reference/convert.md)
