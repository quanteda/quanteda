# Count the number of tokens or types

Get the count of tokens (total features) or types (unique tokens).

## Usage

``` r
ntoken(x, ...)

ntype(x, ...)
```

## Arguments

- x:

  a quanteda [tokens](https://quanteda.io/reference/tokens.md) or
  [dfm](https://quanteda.io/reference/dfm.md) object

- ...:

  additional arguments passed to
  [`tokens()`](https://quanteda.io/reference/tokens.md)

## Value

`ntoken()` returns a named integer vector of the counts of the total
tokens

`ntypes()` returns a named integer vector of the counts of the types
(unique tokens) per document. For
[dfm](https://quanteda.io/reference/dfm.md) objects, `ntype()` will only
return the count of features that occur more than zero times in the dfm.

## Examples

``` r
# simple example
txt <- c(text1 = "This is a sentence, this.", text2 = "A word. Repeated repeated.")
toks <- tokens(txt)
ntoken(toks)
#> text1 text2 
#>     7     6 
ntype(toks)
#> text1 text2 
#>     7     5 
ntoken(tokens_tolower(toks))  # same
#> text1 text2 
#>     7     6 
ntype(tokens_tolower(toks))   # fewer types
#> text1 text2 
#>     6     4 

# with some real texts
toks <- tokens(corpus_subset(data_corpus_inaugural, Year < 1806))
ntoken(tokens(toks, remove_punct = TRUE))
#> 1789-Washington 1793-Washington      1797-Adams  1801-Jefferson  1805-Jefferson 
#>            1430             135            2318            1726            2166 
ntype(tokens(toks, remove_punct = TRUE))
#> 1789-Washington 1793-Washington      1797-Adams  1801-Jefferson  1805-Jefferson 
#>             617              91             819             711             799 
ntoken(dfm(toks))
#> 1789-Washington 1793-Washington      1797-Adams  1801-Jefferson  1805-Jefferson 
#>            1537             147            2577            1923            2380 
ntype(dfm(toks))
#> 1789-Washington 1793-Washington      1797-Adams  1801-Jefferson  1805-Jefferson 
#>             603              95             801             687             781 
```
