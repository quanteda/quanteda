# Combine documents in corpus by a grouping variable

Combine documents in a [corpus](https://quanteda.io/reference/corpus.md)
object by a grouping variable, by concatenating their texts in the order
of the documents within each grouping variable.

## Usage

``` r
corpus_group(x, groups = docid(x), fill = FALSE, concatenator = " ")
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md) object

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

- concatenator:

  the concatenation character that will connect the grouped documents.

## Value

a [corpus](https://quanteda.io/reference/corpus.md) object whose
documents are equal to the unique group combinations, and whose texts
are the concatenations of the texts by group. Document-level variables
that have no variation within groups are saved in
[docvars](https://quanteda.io/reference/docvars.md). Document-level
variables that are lists are dropped from grouping, even when these
exhibit no variation within groups.

## Examples

``` r
corp <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"),
               docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
corpus_group(corp, groups = grp)
#> Corpus consisting of 2 documents and 1 docvar.
#> grp1 :
#> "a a b a b c c"
#> 
#> grp2 :
#> "a c d d a c c d"
#> 
corpus_group(corp, groups = c(1, 1, 2, 2))
#> Corpus consisting of 2 documents and 1 docvar.
#> 1 :
#> "a a b a b c c"
#> 
#> 2 :
#> "a c d d a c c d"
#> 
corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3))
#> Corpus consisting of 2 documents and 1 docvar.
#> 1 :
#> "a a b a b c c"
#> 
#> 2 :
#> "a c d d a c c d"
#> 

# with fill
corpus_group(corp, groups = factor(c(1, 1, 2, 2), levels = 1:3), fill = TRUE)
#> Corpus consisting of 3 documents and 1 docvar.
#> 1 :
#> "a a b a b c c"
#> 
#> 2 :
#> "a c d d a c c d"
#> 
#> 3 :
#> ""
#> 
```
