# Convenience wrappers for dfm convert

To make the usage as consistent as possible with other packages,
quanteda also provides shortcut wrappers to
[`convert()`](https://quanteda.io/reference/convert.md), designed to be
similar in syntax to analogous commands in the packages to whose format
they are converting.

## Usage

``` r
dfm2austin(x)

dfm2tm(x, weighting = tm::weightTf)

dfm2lda(x, omit_empty = TRUE)

dtm2lda(x, omit_empty = TRUE)

dfm2dtm(x, omit_empty = TRUE)

dfm2stm(x, docvars = NULL, omit_empty = TRUE)
```

## Arguments

- x:

  the dfm to be converted

- weighting:

  a tm weight, see
  [`tm::weightTf()`](https://rdrr.io/pkg/tm/man/weightTf.html)

- omit_empty:

  logical; if `TRUE`, omit empty documents and features from the
  converted dfm. This is required for some formats (such as STM) that do
  not accept empty documents. Only used when `to = "lda"` or
  `to = "topicmodels"`. For `to = "stm"` format, `omit_empty` is always
  `TRUE`.

- docvars:

  optional data.frame of document variables used as the `meta`
  information in conversion to the stm package format. This aids in
  selecting the document variables only corresponding to the documents
  with non-zero counts. Only affects the "stm" format.

## Value

A converted object determined by the value of `to` (see above). See
conversion target package documentation for more detailed descriptions
of the return formats.

## Details

`dfm2lda` provides converts a
[dfm](https://quanteda.io/reference/dfm.md) into the list representation
of terms in documents used by the lda package (a list with components
"documents" and "vocab" as needed by
[`lda::lda.collapsed.gibbs.sampler()`](https://rdrr.io/pkg/lda/man/lda.collapsed.gibbs.sampler.html)).

`dfm2ldaformat` provides converts a
[dfm](https://quanteda.io/reference/dfm.md) into the list representation
of terms in documents used by the lda package (a list with components
"documents" and "vocab" as needed by
[`lda::lda.collapsed.gibbs.sampler()`](https://rdrr.io/pkg/lda/man/lda.collapsed.gibbs.sampler.html)).

## Note

Additional coercion methods to base R objects are also available:

- `[as.data.frame](x)`:

  converts a [dfm](https://quanteda.io/reference/dfm.md) into a
  [data.frame](https://rdrr.io/r/base/data.frame.html)

- `[as.matrix](x)`:

  converts a [dfm](https://quanteda.io/reference/dfm.md) into a
  [matrix](https://rdrr.io/r/base/matrix.html)

## Examples

``` r
dfmat <- corpus_subset(data_corpus_inaugural, Year > 1970) |>
    tokens() |>
    dfm()

if (FALSE) { # \dontrun{
# shortcut conversion to lda package list format
identical(quanteda:::dfm2lda(dfmat), convert(dfmat, to = "lda"))
} # }

if (FALSE) { # \dontrun{
# shortcut conversion to lda package list format
identical(dfm2ldaformat(dfmat), convert(dfmat, to = "lda"))
} # }
```
