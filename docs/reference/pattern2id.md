# Match patterns against token types

Developer function to match regex, fixed or glob patterns against token
types. This allows C++ function to perform fast searches in tokens
object. C++ functions use a list of type IDs to construct a hash table,
against which sub-vectors of tokens object are matched. This function
constructs an index of glob patterns for faster matching.

`pattern2fixed` converts regex and glob patterns to fixed patterns.

## Usage

``` r
pattern2id(
  pattern,
  types,
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE,
  keep_nomatch = FALSE,
  use_index = TRUE
)

pattern2fixed(
  pattern,
  types,
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE,
  keep_nomatch = FALSE,
  use_index = TRUE
)
```

## Arguments

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- types:

  token types against which patterns are matched

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- keep_nomatch:

  keep patterns that did not match

- use_index:

  construct index of types for quick search

## Value

a list of integer vectors containing indices of matched types

`pattern2fixed` returns a list of character vectors containing types

## Examples

``` r
types <- c("A", "AA", "B", "BB", "BBB", "C", "CC")

pats_regex <- list(c("^a$", "^b"), c("c"), c("d"))
pattern2id(pats_regex, types, "regex", case_insensitive = TRUE)
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 1 4
#> 
#> [[3]]
#> [1] 1 5
#> 
#> [[4]]
#> [1] 6
#> 
#> [[5]]
#> [1] 7
#> 

pats_glob <- list(c("a*", "b*"), c("c"), c("d"))
pattern2id(pats_glob, types, "glob", case_insensitive = TRUE)
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> [1] 1 4
#> 
#> [[4]]
#> [1] 2 4
#> 
#> [[5]]
#> [1] 1 5
#> 
#> [[6]]
#> [1] 2 5
#> 
#> [[7]]
#> [1] 6
#> 

pattern <- list(c("^a$", "^b"), c("c"), c("d"))
types <- c("A", "AA", "B", "BB", "BBB", "C", "CC")
pattern2fixed(pattern, types, "regex", case_insensitive = TRUE)
#> [[1]]
#> [1] "A" "B"
#> 
#> [[2]]
#> [1] "A"  "BB"
#> 
#> [[3]]
#> [1] "A"   "BBB"
#> 
#> [[4]]
#> [1] "C"
#> 
#> [[5]]
#> [1] "CC"
#> 
```
