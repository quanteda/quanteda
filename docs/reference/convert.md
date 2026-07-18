# Convert quanteda objects to non-quanteda formats

Convert a quanteda [dfm](https://quanteda.io/reference/dfm.md) or
[corpus](https://quanteda.io/reference/corpus.md) object to a format
useable by other packages. The general function `convert` provides easy
conversion from a dfm to the document-term representations used in all
other text analysis packages for which conversions are defined. For
[corpus](https://quanteda.io/reference/corpus.md) objects, `convert`
provides an easy way to make a corpus and its document variables into a
data.frame.

## Usage

``` r
convert(x, to, ...)

# S3 method for class 'dfm'
convert(
  x,
  to = c("lda", "tm", "stm", "austin", "topicmodels", "lsa", "matrix", "data.frame",
    "tripletlist"),
  docvars = NULL,
  omit_empty = TRUE,
  docid_field = "doc_id",
  ...
)

# S3 method for class 'corpus'
convert(x, to = c("data.frame", "json"), pretty = FALSE, ...)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md) or
  [corpus](https://quanteda.io/reference/corpus.md) to be converted

- to:

  target conversion format, one of:

  `"lda"`

  :   a list with components "documents" and "vocab" as needed by the
      function
      [lda.collapsed.gibbs.sampler](https://rdrr.io/pkg/lda/man/lda.collapsed.gibbs.sampler.html)
      from the lda package

  `"tm"`

  :   a DocumentTermMatrix from the tm package. Note: The tm package
      version of `as.TermDocumentMatrix()` allows a `weighting`
      argument, which supplies a weighting function for
      `TermDocumentMatrix()`. Here the default is for term frequency
      weighting. If you want a different weighting, apply the weights
      after converting using one of the tm functions. For other
      available weighting functions from the tm package, see
      TermDocumentMatrix.

  `"stm"`

  :   the format for the stm package

  `"austin"`

  :   the `wfm` format from the **austin** package

  `"topicmodels"`

  :   the "dtm" format as used by the topicmodels package

  `"lsa"`

  :   the "textmatrix" format as used by the lsa package

  `"data.frame"`

  :   a data.frame of without row.names, in which documents are rows,
      and each feature is a variable (for a dfm), or each text and its
      document variables form a row (for a corpus)

  `"json"`

  :   (corpus only) convert a corpus and its document variables into
      JSON format, using the format described in
      [jsonlite::toJSON()](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

  `"tripletlist"`

  :   a named "triplet" format list consisting of `document`, `feature`,
      and `frequency`

- ...:

  unused directly

- docvars:

  optional data.frame of document variables used as the `meta`
  information in conversion to the stm package format. This aids in
  selecting the document variables only corresponding to the documents
  with non-zero counts. Only affects the "stm" format.

- omit_empty:

  logical; if `TRUE`, omit empty documents and features from the
  converted dfm. This is required for some formats (such as STM) that do
  not accept empty documents. Only used when `to = "lda"` or
  `to = "topicmodels"`. For `to = "stm"` format, `omit_empty` is always
  `TRUE`.

- docid_field:

  character; the name of the column containing document names used when
  `to = "data.frame"`. Unused for other conversions.

- pretty:

  adds indentation whitespace to JSON output. Can be TRUE/FALSE or a
  number specifying the number of spaces to indent (default is 2). Use a
  negative number for tabs instead of spaces.

## Value

A converted object determined by the value of `to` (see above). See
conversion target package documentation for more detailed descriptions
of the return formats.

## Examples

``` r
## convert a dfm

toks <- corpus_subset(data_corpus_inaugural, Year > 1970) |>
    tokens()
dfmat1 <- dfm(toks)

# austin's wfm format
identical(dim(dfmat1), dim(convert(dfmat1, to = "austin")))
#> [1] TRUE

# stm package format
stmmat <- convert(dfmat1, to = "stm")
str(stmmat)
#> List of 3
#>  $ documents:List of 14
#>   ..$ 1973-Nixon  : int [1:2, 1:515] 2 2 6 96 7 17 8 69 21 1 ...
#>   ..$ 1977-Carter : int [1:2, 1:501] 2 4 4 1 5 1 6 65 7 12 ...
#>   ..$ 1981-Reagan : int [1:2, 1:850] 2 20 6 174 7 10 8 130 20 1 ...
#>   ..$ 1985-Reagan : int [1:2, 1:876] 2 6 3 1 4 1 5 1 6 177 ...
#>   ..$ 1989-Bush   : int [1:2, 1:756] 2 6 6 166 7 8 8 142 26 2 ...
#>   ..$ 1993-Clinton: int [1:2, 1:605] 2 4 6 139 8 81 31 1 46 5 ...
#>   ..$ 1997-Clinton: int [1:2, 1:726] 2 4 6 131 7 13 8 108 19 1 ...
#>   ..$ 2001-Bush   : int [1:2, 1:592] 2 2 6 110 7 2 8 96 32 1 ...
#>   ..$ 2005-Bush   : int [1:2, 1:734] 2 6 3 1 6 120 7 2 8 98 ...
#>   ..$ 2009-Obama  : int [1:2, 1:900] 1 1 2 2 6 130 7 22 8 118 ...
#>   ..$ 2013-Obama  : int [1:2, 1:786] 6 99 7 13 8 89 13 1 26 1 ...
#>   ..$ 2017-Trump  : int [1:2, 1:547] 2 2 3 1 6 96 7 11 8 88 ...
#>   ..$ 2021-Biden  : int [1:2, 1:744] 2 10 6 147 8 210 9 1 10 1 ...
#>   ..$ 2025-Trump  : int [1:2, 1:950] 4 4 5 4 6 221 8 179 15 1 ...
#>  $ vocab    : chr [1:3934] "!" "\"" "'" "(" ...
#>  $ meta     :'data.frame':   14 obs. of  4 variables:
#>   ..$ Year     : int [1:14] 1973 1977 1981 1985 1989 1993 1997 2001 2005 2009 ...
#>   ..$ President: chr [1:14] "Nixon" "Carter" "Reagan" "Reagan" ...
#>   ..$ FirstName: chr [1:14] "Richard Milhous" "Jimmy" "Ronald" "Ronald" ...
#>   ..$ Party    : Factor w/ 6 levels "Democratic","Democratic-Republican",..: 5 1 5 5 5 1 1 5 5 1 ...

# triplet
tripletmat <- convert(dfmat1, to = "tripletlist")
str(tripletmat)
#> List of 3
#>  $ document : chr [1:10082] "1973-Nixon" "1981-Reagan" "1989-Bush" "2005-Bush" ...
#>  $ feature  : chr [1:10082] "mr" "mr" "mr" "mr" ...
#>  $ frequency: num [1:10082] 3 3 6 1 1 69 52 130 124 142 ...

if (FALSE) { # \dontrun{
# tm's DocumentTermMatrix format
tmdfm <- convert(dfmat1, to = "tm")
str(tmdfm)

# topicmodels package format
str(convert(dfmat1, to = "topicmodels"))

# lda package format
str(convert(dfmat1, to = "lda"))
} # }

## convert a corpus into a data.frame

corp <- corpus(c(d1 = "Text one.", d2 = "Text two."),
               docvars = data.frame(dvar1 = 1:2, dvar2 = c("one", "two"),
                                    stringsAsFactors = FALSE))
convert(corp, to = "data.frame")
#>   doc_id      text dvar1 dvar2
#> 1     d1 Text one.     1   one
#> 2     d2 Text two.     2   two
convert(corp, to = "json")
#> [{"doc_id":"d1","text":"Text one.","dvar1":1,"dvar2":"one"},{"doc_id":"d2","text":"Text two.","dvar1":2,"dvar2":"two"}] 
```
