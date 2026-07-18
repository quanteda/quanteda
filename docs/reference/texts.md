# Get or assign corpus texts \[deprecated\]

**\[superseded\]**

This function has been made defunct and replaced.

- Use
  [`as.character.corpus()`](https://quanteda.io/reference/as.character.corpus.md)
  to turn a corpus into a simple named character vector.

- Use [`corpus_group()`](https://quanteda.io/reference/corpus_group.md)
  instead of `texts(x, groups = ...)` to aggregate texts by a grouping
  variable.

- Use `[<-` instead of `texts()<-` for replacing texts in a corpus
  object.

## Usage

``` r
texts(x, groups = NULL, spacer = " ")

texts(x) <- value
```

## Arguments

- x:

  a [corpus](https://quanteda.io/reference/corpus.md)

- groups:

  grouping variable for sampling, equal in length to the number of
  documents. This will be evaluated in the docvars data.frame, so that
  docvars may be referred to by name without quoting. This also changes
  previous behaviours for `groups`. See
  `news(Version >= "3.0", package = "quanteda")` for details.

- spacer:

  when concatenating texts by using `groups`, this will be the spacing
  added between texts. (Default is two spaces.)

- value:

  character vector of the new texts

## Value

For `texts`, a character vector of the texts in the corpus.

For `texts <-`, the corpus with the updated texts.

for `texts <-`, a corpus with the texts replaced by `value`

## Details

Get or replace the texts in a
[corpus](https://quanteda.io/reference/corpus.md), with grouping
options. Works for plain character vectors too, if `groups` is a factor.

## Note

The `groups` will be used for concatenating the texts based on shared
values of `groups`, without any specified order of aggregation.

You are strongly encouraged as a good practice of text analysis workflow
*not* to modify the substance of the texts in a corpus. Rather, this
sort of processing is better performed through downstream operations.
For instance, do not lowercase the texts in a corpus, or you will never
be able to recover the original case. Rather, apply
[`tokens_tolower()`](https://quanteda.io/reference/tokens_tolower.md)
after applying [`tokens()`](https://quanteda.io/reference/tokens.md) to
a corpus, or use the option `tolower = TRUE` in
[`dfm()`](https://quanteda.io/reference/dfm.md).
