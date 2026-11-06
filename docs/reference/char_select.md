# Select or remove elements from a character vector

These function select or discard elements from a
[character](https://rdrr.io/r/base/character.html) object. For
convenience, the functions `char_remove` and `char_keep` are defined as
shortcuts for `char_select(x, pattern, selection = "remove")` and
`char_select(x, pattern, selection = "keep")`, respectively.

These functions make it easy to change, for instance, stopwords based on
pattern matching.

## Usage

``` r
char_select(
  x,
  pattern,
  selection = c("keep", "remove"),
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE
)

char_remove(x, ...)

char_keep(x, ...)
```

## Arguments

- x:

  an input [character](https://rdrr.io/r/base/character.html) vector

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- selection:

  whether to `"keep"` or `"remove"` the tokens matching `pattern`

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- ...:

  additional arguments passed by `char_remove` and `char_keep` to
  `char_select`. Cannot include `selection`.

## Value

a modified [character](https://rdrr.io/r/base/character.html) vector

## Examples

``` r
# character selection
mykeywords <- c("natural", "national", "denatured", "other")
char_select(mykeywords, "nat*", valuetype = "glob")
#> [1] "natural"  "national"
char_select(mykeywords, "nat", valuetype = "regex")
#> [1] "natural"   "national"  "denatured"
char_select(mykeywords, c("natur*", "other"))
#> [1] "natural" "other"  
char_select(mykeywords, c("natur*", "other"), selection = "remove")
#> [1] "national"  "denatured"

# character removal
char_remove(letters[1:5], c("a", "c", "x"))
#> [1] "b" "d" "e"
words <- c("any", "and", "Anna", "as", "announce", "but")
char_remove(words, "an*")
#> [1] "as"  "but"
char_remove(words, "an*", case_insensitive = FALSE)
#> [1] "Anna" "as"   "but" 
char_remove(words, "^.n.+$", valuetype = "regex")
#> [1] "as"  "but"

# remove some of the system stopwords
stopwords("en", source = "snowball")[1:6]
#> [1] "i"      "me"     "my"     "myself" "we"     "our"   
stopwords("en", source = "snowball")[1:6] |>
  char_remove(c("me", "my*"))
#> [1] "i"   "we"  "our"
  
# character keep
char_keep(letters[1:5], c("a", "c", "x"))
#> [1] "a" "c"
```
