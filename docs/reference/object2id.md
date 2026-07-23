# Match quanteda objects against token types

Developer function to match patterns in quanteda objects against token
types.

## Usage

``` r
object2id(
  x,
  types,
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE,
  concatenator = "_",
  levels = 1,
  match_pattern = c("any", "single", "multi"),
  keep_nomatch = FALSE
)

object2fixed(
  x,
  types,
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE,
  concatenator = "_",
  levels = 1,
  match_pattern = c("any", "single", "multi"),
  keep_nomatch = FALSE
)
```

## Arguments

- x:

  a list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md) or
  collocations object

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

- concatenator:

  the concatenation character that joins multi-word expression in
  `types`

- levels:

  integers specifying the levels of entries in a hierarchical dictionary
  that will be applied. The top level is 1, and subsequent levels
  describe lower nesting levels. Values may be combined, even if these
  levels are not contiguous, e.g. `levels = c(1:3)` will collapse the
  second level into the first, but record the third level (if present)
  collapsed below the first (see examples).

- match_pattern:

  select only single-word patterns or multi-word patterns should be
  matched. If "any", it matches both single-word and multi-word
  patterns.

- keep_nomatch:

  keep patterns that did not match

## Value

`object2fixed()` returns a list of character vectors of matched types.
`object2id()` returns a list of indices of matched types with
attributes. The "pattern" attribute records the indices of the matched
patterns in `x`; the "key" attribute records the keys of the matched
patterns when `x` is
[dictionary](https://quanteda.io/reference/dictionary.md).

## See also

[`pattern2id()`](https://quanteda.io/reference/pattern2id.md)

## Examples

``` r
types <- c("A", "AA", "B", "BB", "B_B", "C", "C-C")

# dictionary
dict <- dictionary(list(A = c("a", "aa"), 
                        B = c("BB", "B B"),
                        C = c("C", "C-C")))
object2fixed(dict, types)
#> $A
#> [1] "A"
#> 
#> $A
#> [1] "AA"
#> 
#> $B
#> [1] "BB"
#> 
#> $B
#> [1] "B" "B"
#> 
#> $B
#> [1] "B_B"
#> 
#> $C
#> [1] "C"
#> 
#> $C
#> [1] "C-C"
#> 
object2fixed(dict, types, match_pattern = "single")
#> $A
#> [1] "A"
#> 
#> $A
#> [1] "AA"
#> 
#> $B
#> [1] "BB"
#> 
#> $B
#> [1] "B_B"
#> 
#> $C
#> [1] "C"
#> 
#> $C
#> [1] "C-C"
#> 
object2fixed(dict, types, match_pattern = "multi")
#> $B
#> [1] "B" "B"
#> 

# phrase
pats <- phrase(c("a", "aa", "zz", "bb", "b b"))
object2fixed(pats, types)
#> $a
#> [1] "A"
#> 
#> $aa
#> [1] "AA"
#> 
#> $bb
#> [1] "BB"
#> 
#> $`b b`
#> [1] "B" "B"
#> 
object2fixed(pats, types, keep_nomatch = TRUE)
#> $a
#> [1] "A"
#> 
#> $aa
#> [1] "AA"
#> 
#> $zz
#> character(0)
#> 
#> $bb
#> [1] "BB"
#> 
#> $`b b`
#> [1] "B" "B"
#> 
```
