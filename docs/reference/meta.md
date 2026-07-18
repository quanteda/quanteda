# Get or set object metadata

Get or set the object metadata in a
[corpus](https://quanteda.io/reference/corpus.md),
[tokens](https://quanteda.io/reference/tokens.md),
[dfm](https://quanteda.io/reference/dfm.md), or
[dictionary](https://quanteda.io/reference/dictionary.md) object. With
the exception of dictionaries, this will be corpus-level metadata.

## Usage

``` r
meta(x, field = NULL, type = c("user", "object", "system", "all"))

meta(x, field = NULL) <- value
```

## Arguments

- x:

  an object for which the metadata will be read or set

- field:

  metadata field name(s); if `NULL` (default), return all metadata names

- type:

  `"user"` for user-provided corpus-level metadata; `"system"` for
  metadata set automatically when the corpus is created; or `"all"` for
  all metadata.

- value:

  new value of the metadata field

## Value

For `meta`, a named list of the metadata fields in the corpus.

For `meta <-`, the corpus with the updated user-level metadata. Only
user-level metadata may be assigned.

## Examples

``` r
meta(data_corpus_inaugural)
#> $description
#> [1] "Transcripts of all inaugural addresses delivered by United States Presidents, from Washington 1789 onward.  Data compiled by Gerhard Peters."
#> 
#> $source
#> [1] "Gerhard Peters and John T. Woolley. The American Presidency Project."
#> 
#> $url
#> [1] "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/inaugural-addresses"
#> 
#> $author
#> [1] "(various US Presidents)"
#> 
#> $keywords
#> [1] "political"     "US politics"   "United States" "presidents"   
#> [5] "presidency"   
#> 
#> $title
#> [1] "US presidential inaugural address speeches"
#> 
meta(data_corpus_inaugural, "source")
#> [1] "Gerhard Peters and John T. Woolley. The American Presidency Project."
meta(data_corpus_inaugural, "citation") <- "Presidential Speeches Online Project (2014)."
meta(data_corpus_inaugural, "citation")
#> [1] "Presidential Speeches Online Project (2014)."
```
