# Select types without performing slow regex search

This is an internal function for `pattern2id` that select types using
keys in index when available.

`index_types` is an internal function for `pattern2id` that constructs
an index of "glob" or "fixed" patterns to avoid expensive sequential
search.

## Usage

``` r
search_glob(pattern, types_search, case_insensitive, index = NULL)

search_glob_multi(patterns, types_search, case_insensitive, index)

search_regex(pattern, types_search, case_insensitive)

search_regex_multi(patterns, types_search, case_insensitive)

search_fixed(pattern, types_search, index = NULL)

search_fixed_multi(patterns, types_search, index)

index_types(
  pattern,
  types,
  valuetype = c("glob", "fixed", "regex"),
  case_insensitive = TRUE
)
```

## Arguments

- pattern:

  a "glob", "fixed" or "regex" pattern

- types_search:

  lowercased types when `case_insensitive=TRUE`, but not used in glob
  and fixed matching as types are in the index.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- index:

  index object created by `index_types`

- patterns:

  a list of "glob", "fixed" or "regex" patterns

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

## Value

`index_types` returns a list of integer vectors containing type IDs with
index keys as an attribute

## Examples

``` r
index <- quanteda:::index_types("yy*", c("xxx", "yyyy", "ZZZ"), "glob", FALSE)
quanteda:::search_glob("yy*", attr(index, "types_search"), index)
#> [1] 2
```
