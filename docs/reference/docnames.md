# Get or set document names

Get or set document names

## Usage

``` r
docnames(x)

docnames(x, unique_docnames = TRUE) <- value

docid(x)

segid(x)
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md),
  [tokens](https://quanteda.io/reference/tokens.md),
  [tokens_xptr](https://quanteda.io/reference/tokens_xptr.md), or
  [dfm](https://quanteda.io/reference/dfm.md) object.

- unique_docnames:

  logical; if `TRUE`, enforce strict uniqueness in `docnames`;
  otherwise, rename duplicated docnames using an added serial number,
  and treat them as segments of the same document.

- value:

  a character vector of the same length as `x`

## Value

`docnames()` returns a character vector of the unique document names.

`docnames <-` assigns new values to the document names of an object.
docnames can only be character, so any non-character value assigned to
be a docname will be coerced to mode `character`.

`docid()` returns an internal factor variable that records the original
`doc_id`. If an object has been reshaped (e.g.
[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md) or
segmented (e.g.
[`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)),
`docid(x)` returns the original `doc_id` but `segid(x)` does the serial
number of those segments within the original `doc_id`.

## Note

`docid()` and `segid()` are designed primarily for developers, not for
end users. In most cases, you will want `docnames()` instead. It is,
however, the default for
[groups](https://quanteda.io/reference/groups.md), so that documents
that have been previously reshaped (e.g.
[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md) or
segmented (e.g.
[`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md))
will be regrouped into their original `doc_id` when `groups = docid(x)`.

## See also

[`featnames()`](https://quanteda.io/reference/featnames.md)

## Examples

``` r
# get and set doument names to a corpus
corp <- data_corpus_inaugural
docnames(corp) <- char_tolower(docnames(corp))

# get and set doument names to a tokens
toks <- tokens(corp)
docnames(toks) <- char_tolower(docnames(toks))

# get and set doument names to a dfm
dfmat <- dfm(tokens(corp))
docnames(dfmat) <- char_tolower(docnames(dfmat))

# reassign the document names of the inaugural speech corpus
corp <- data_corpus_inaugural
docnames(corp) <- paste0("Speech", seq_len(ndoc(corp)))


corp <- corpus(c(textone = "This is a sentence.  Another sentence.  Yet another.",
                 textwo = "Sentence 1. Sentence 2."))
corp_sent <- corp |>
    corpus_reshape(to = "sentences")
docnames(corp_sent)
#> [1] "textone.1" "textone.2" "textone.3" "textwo.1"  "textwo.2" 

# docid
docid(corp_sent)
#> [1] textone textone textone textwo  textwo 
#> Levels: textone textwo
docid(tokens(corp_sent))
#> [1] textone textone textone textwo  textwo 
#> Levels: textone textwo
docid(dfm(tokens(corp_sent)))
#> [1] textone textone textone textwo  textwo 
#> Levels: textone textwo

# segid
segid(corp_sent)
#> [1] 1 2 3 1 2
segid(tokens(corp_sent))
#> [1] 1 2 3 1 2
segid(dfm(tokens(corp_sent)))
#> [1] 1 2 3 1 2
```
