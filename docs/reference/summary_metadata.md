# Functions to add or retrieve corpus summary metadata

Functions to add or retrieve corpus summary metadata

## Usage

``` r
add_summary_metadata(x, extended = FALSE, ...)

get_summary_metadata(x, ...)

summarize_texts_extended(x, stop_words = stopwords("en"), n = 100)
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md) object

- ...:

  additional arguments passed to
  [`tokens()`](https://quanteda.io/reference/tokens.md) when computing
  the summary information

## Value

`add_summary_metadata()` returns a corpus with summary metadata added as
a data.frame, with the top-level list element names
[`summary()`](https://rdrr.io/r/base/summary.html).

`get_summary_metadata()` returns the summary metadata as a data.frame.

`summarize_texts_extended()` returns extended summary information.

## Details

This is provided so that a
[corpus](https://quanteda.io/reference/corpus.md) object can be stored
with summary information to avoid having to compute this every time
[`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md) is
called.

So in future calls, if
`!is.null(meta(x, "summary", type = "system") && !length(list(...))`,
then
[`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md)
will simply return `get_system_meta()` rather than compute the summary
statistics on the fly, which requires tokenizing the text.

## Examples

``` r
corp <- corpus(data_char_ukimmig2010)
corp <- quanteda:::add_summary_metadata(corp)
quanteda:::get_summary_metadata(corp)
#> Corpus consisting of 9 documents, showing 9 documents:
#> 
#>          Text Types Tokens Sentences
#>           BNP  1125   3280       136
#>     Coalition   142    260        12
#>  Conservative   251    499        21
#>        Greens   322    679        30
#>        Labour   298    683        33
#>        LibDem   251    483        26
#>            PC    77    114         5
#>           SNP    88    134         4
#>          UKIP   346    723        37
#> 

## using extended summary

if (FALSE) { # \dontrun{
extended_data <- quanteda:::summarize_texts_extended(data_corpus_inaugural)
topfeatures(extended_data$top_dfm)
} # }
```
