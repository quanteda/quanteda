# Locate a pattern in a tokens object

Locates a [pattern](https://quanteda.io/reference/pattern.md) within a
tokens object, returning the index positions of the beginning and ending
tokens in the pattern.

## Usage

``` r
index(
  x,
  pattern,
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE
)

is.index(x)
```

## Arguments

- x:

  an input [tokens](https://quanteda.io/reference/tokens.md) object

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

## Value

a data.frame consisting of one row per pattern match, with columns for
the document name, index positions `from` and `to`, and the pattern
matched.

`is.index` returns `TRUE` if the object was created by `index()`;
`FALSE` otherwise.

## Examples

``` r
toks <- tokens(data_corpus_inaugural[1:8])
index(toks, pattern = "secure*")
#>          docname from   to pattern
#> 1     1797-Adams  478  478 secure*
#> 2     1797-Adams 1512 1512 secure*
#> 3 1805-Jefferson 2367 2367 secure*
#> 4    1817-Monroe 1754 1754 secure*
#> 5    1817-Monroe 1814 1814 secure*
#> 6    1817-Monroe 3009 3009 secure*
index(toks, pattern = c("secure*", phrase("united states"))) |> head()
#>           docname from   to       pattern
#> 1 1789-Washington  433  434 united states
#> 2 1789-Washington  529  530 united states
#> 3      1797-Adams  478  478       secure*
#> 4      1797-Adams  524  525 united states
#> 5      1797-Adams 1512 1512       secure*
#> 6      1797-Adams 1716 1717 united states
```
