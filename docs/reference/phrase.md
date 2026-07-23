# Declare a pattern to be a sequence of separate patterns

Declares that a character expression consists of multiple patterns,
separated by an element such as whitespace. This is typically used as a
wrapper around [`pattern()`](https://quanteda.io/reference/pattern.md)
to make it explicit that the pattern elements are to be used for matches
to multi-word sequences, rather than individual, unordered matches to
single words.

## Usage

``` r
phrase(x, separator = " ")

as.phrase(x)

is.phrase(x)
```

## Arguments

- x:

  character, [dictionary](https://quanteda.io/reference/dictionary.md),
  list, collocations, or tokens object; the compound patterns to be
  treated as a sequence separated by `separator`. For list,
  collocations, or tokens objects, use `as.phrase()`.

- separator:

  character; the character in between the patterns. This defaults to "
  ". For `phrase()` only.

## Value

`phrase()` and `as.phrase()` return a specially classed list whose
elements have been split into separate `character` (pattern) elements.

`is.phrase` returns `TRUE` if the object was created by `phrase()`;
`FALSE` otherwise.

## See also

`as.phrase()`

## Examples

``` r
# make phrases from characters
phrase(c("natural language processing"))
#> [[1]]
#> [1] "natural"    "language"   "processing"
#> 
phrase(c("natural_language_processing", "text_analysis"), separator = "_")
#> [[1]]
#> [1] "natural"    "language"   "processing"
#> 
#> [[2]]
#> [1] "text"     "analysis"
#> 

# from a dictionary
phrase(dictionary(list(catone = c("a b"), cattwo = "c d e", catthree = "f")))
#> [[1]]
#> [1] "a" "b"
#> 
#> [[2]]
#> [1] "c" "d" "e"
#> 
#> [[3]]
#> [1] "f"
#> 

# from a list
as.phrase(list(c("natural", "language", "processing")))
#> [[1]]
#> [1] "natural"    "language"   "processing"
#> 

# from tokens
as.phrase(tokens("natural language processing"))
#> [[1]]
#> [1] "natural"    "language"   "processing"
#> 
```
