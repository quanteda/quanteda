# Grouping variable(s) for various functions

Groups for aggregation by various functions that take grouping options.

## Arguments

- groups:

  grouping variable for sampling, equal in length to the number of
  documents. This will be evaluated in the docvars data.frame, so that
  docvars may be referred to by name without quoting. This also changes
  previous behaviours for `groups`. See
  `news(Version >= "3.0", package = "quanteda")` for details.

- fill:

  logical; if `TRUE` and `groups` is a factor, then use all levels of
  the factor when forming the new documents of the grouped object. This
  will result in a new "document" with empty content for levels not
  observed, but for which an empty document may be needed. If `groups`
  is a factor of dates, for instance, then `fill = TRUE` ensures that
  the new object will consist of one new "document" by date, regardless
  of whether any documents previously existed with that date. Has no
  effect if the `groups` variable(s) are not factors.

## See also

[`corpus_group()`](https://quanteda.io/reference/corpus_group.md),
[`tokens_group()`](https://quanteda.io/reference/tokens_group.md),
[`dfm_group()`](https://quanteda.io/reference/dfm_group.md)
