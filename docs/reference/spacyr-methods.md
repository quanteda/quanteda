# Extensions for and from spacy_parse objects

These functions provide quanteda methods for spacyr objects, and also
extend [spacy_parse](http://spacyr.quanteda.io/reference/spacy_parse.md)
and
[spacy_tokenize](http://spacyr.quanteda.io/reference/spacy_tokenize.md)
to work directly with [corpus](https://quanteda.io/reference/corpus.md)
objects.

## Arguments

- x:

  an object returned by `spacy_parse`, or (for `spacy_parse`) a
  [corpus](https://quanteda.io/reference/corpus.md) object

- ...:

  not used for these functions

## Details

`spacy_parse(x, ...)` and `spacy_tokenize(x, ...)` work directly on
quanteda [corpus](https://quanteda.io/reference/corpus.md) objects.

`docnames(x)` returns the document names

`ndoc(x)` returns the number of documents

`ntoken(x, ...)` returns the number of tokens by document

`ntype(x, ...)` returns the number of types (unique tokens) by document

`nsentence(x)` returns the number of sentences by document

## Examples

``` r
if (FALSE) { # \dontrun{
library("spacyr")
spacy_initialize()

corp <- corpus(c(doc1 = "And now, now, now for something completely different.",
                 doc2 = "Jack and Jill are children."))
spacy_tokenize(corp)
(parsed <- spacy_parse(corp))

ntype(parsed)
ntoken(parsed)
ndoc(parsed)
docnames(parsed)
} # }
```
