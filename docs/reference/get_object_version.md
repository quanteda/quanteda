# Get the package version that created an object

Return the the quanteda package version in which a
[dfm](https://quanteda.io/reference/dfm.md),
[tokens](https://quanteda.io/reference/tokens.md), or
[corpus](https://quanteda.io/reference/corpus.md) object was created.

## Usage

``` r
get_object_version(x)

is_pre2(x)
```

## Value

A three-element integer vector of class "package_version". For versions
of the package \< 1.5 for which no version was recorded in the object,
`c(1, 4, 0)` is returned.

`ispre2()` returns `TRUE` if the object was created before quanteda
version 2, or `FALSE` otherwise
