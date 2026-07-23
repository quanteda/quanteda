# Base method extensions for corpus objects

Extensions of base R functions for corpus objects.

## Usage

``` r
# S3 method for class 'corpus'
c1 + c2

# S3 method for class 'corpus'
c(..., recursive = FALSE)

# S3 method for class 'corpus'
x[i, drop_docid = TRUE]

# S3 method for class 'summary.corpus'
print(x, ...)
```

## Arguments

- c1:

  corpus one to be added

- c2:

  corpus two to be added

- recursive:

  logical used by [`c()`](https://rdrr.io/r/base/c.html) method, always
  set to `FALSE`

- x:

  a corpus object

- i:

  document names or indices for documents to extract.

- drop_docid:

  if `TRUE`, drop `docid` for documents removed as the result of
  extraction.

## Value

The `+` and [`c()`](https://rdrr.io/r/base/c.html) operators return a
[`corpus()`](https://quanteda.io/reference/corpus.md) object.

Indexing a corpus works in three ways, as of v2.x.x:

- `[` returns a subsetted corpus

- `[[` returns the textual contents of a subsetted corpus (similar to
  [`as.character()`](https://rdrr.io/r/base/character.html))

- `$` returns a vector containing the single named
  [docvars](https://quanteda.io/reference/docvars.md)

## Details

The `+` operator for a corpus object will combine two corpus objects,
resolving any non-matching
[`docvars()`](https://quanteda.io/reference/docvars.md) by making them
into `NA` values for the corpus lacking that field. Corpus-level meta
data is concatenated, except for `source` and `notes`, which are stamped
with information pertaining to the creation of the new joined corpus.

The [`c()`](https://rdrr.io/r/base/c.html) operator is also defined for
corpus class objects, and provides an easy way to combine multiple
corpus objects.

There are some issues that need to be addressed in future revisions of
quanteda concerning the use of factors to store document variables and
meta-data. Currently most or all of these are not recorded as factors,
because we use `stringsAsFactors=FALSE` in the
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) calls that are
used to create and store the document-level information, because the
texts should always be stored as character vectors and never as factors.

## See also

[`summary.corpus()`](https://quanteda.io/reference/summary.corpus.md)

## Examples

``` r
# concatenate corpus objects
corp1 <- corpus(data_char_ukimmig2010[1:2])
corp2 <- corpus(data_char_ukimmig2010[3:4])
corp3 <- corpus(data_char_ukimmig2010[5:6])
summary(c(corp1, corp2, corp3))
#> Corpus consisting of 6 documents, showing 6 documents:
#> 
#>          Text Types Tokens Sentences
#>           BNP  1125   3280       136
#>     Coalition   142    260        12
#>  Conservative   251    499        21
#>        Greens   322    679        30
#>        Labour   298    683        33
#>        LibDem   251    483        26
#> 

# two ways to index corpus elements
data_corpus_inaugural["1793-Washington"]
#> Corpus consisting of 1 document and 4 docvars.
#> 1793-Washington :
#> "Fellow citizens, I am again called upon by the voice of my c..."
#> 
data_corpus_inaugural[2]
#> Corpus consisting of 1 document and 4 docvars.
#> 1793-Washington :
#> "Fellow citizens, I am again called upon by the voice of my c..."
#> 

# return the text itself
data_corpus_inaugural[["1793-Washington"]]
#> [1] "Fellow citizens, I am again called upon by the voice of my country to execute the functions of its Chief Magistrate. When the occasion proper for it shall arrive, I shall endeavor to express the high sense I entertain of this distinguished honor, and of the confidence which has been reposed in me by the people of united America.\n\nPrevious to the execution of any official act of the President the Constitution requires an oath of office. This oath I am now about to take, and in your presence: That if it shall be found during my administration of the Government I have in any instance violated willingly or knowingly the injunctions thereof, I may (besides incurring constitutional punishment) be subject to the upbraidings of all who are now witnesses of the present solemn ceremony.\n\n "
```
