# Extract a subset of a corpus

Returns subsets of a corpus that meet certain conditions, including
direct logical operations on docvars (document-level variables).
`corpus_subset` functions identically to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html), using
non-standard evaluation to evaluate conditions based on the
[docvars](https://quanteda.io/reference/docvars.md) in the corpus.

## Usage

``` r
corpus_subset(x, subset, drop_docid = TRUE, ...)
```

## Arguments

- x:

  [corpus](https://quanteda.io/reference/corpus.md) object to be
  subsetted.

- subset:

  logical expression indicating the documents to keep: missing values
  are taken as false.

- drop_docid:

  if `TRUE`, `docid` for documents are removed as the result of
  subsetting.

- ...:

  not used

## Value

corpus object, with a subset of documents (and docvars) selected
according to arguments

## See also

[`subset.data.frame()`](https://rdrr.io/r/base/subset.html)

## Examples

``` r
summary(corpus_subset(data_corpus_inaugural, Year > 1980))
#> Corpus consisting of 12 documents, showing 12 documents:
#> 
#>          Text Types Tokens Sentences Year President FirstName      Party
#>   1981-Reagan   902   2781       130 1981    Reagan    Ronald Republican
#>   1985-Reagan   925   2909       126 1985    Reagan    Ronald Republican
#>     1989-Bush   795   2674       144 1989      Bush    George Republican
#>  1993-Clinton   642   1833        82 1993   Clinton      Bill Democratic
#>  1997-Clinton   773   2436       113 1997   Clinton      Bill Democratic
#>     2001-Bush   621   1806        98 2001      Bush George W. Republican
#>     2005-Bush   772   2312        99 2005      Bush George W. Republican
#>    2009-Obama   938   2689       112 2009     Obama    Barack Democratic
#>    2013-Obama   814   2317        90 2013     Obama    Barack Democratic
#>    2017-Trump   582   1660        89 2017     Trump Donald J. Republican
#>    2021-Biden   812   2766       229 2021     Biden Joseph R. Democratic
#>    2025-Trump  1000   3347       177 2025     Trump Donald J. Republican
#> 
summary(corpus_subset(data_corpus_inaugural, Year > 1930 & President == "Roosevelt"))
#> Corpus consisting of 4 documents, showing 4 documents:
#> 
#>            Text Types Tokens Sentences Year President   FirstName      Party
#>  1933-Roosevelt   743   2057        86 1933 Roosevelt Franklin D. Democratic
#>  1937-Roosevelt   725   1989        96 1937 Roosevelt Franklin D. Democratic
#>  1941-Roosevelt   526   1519        68 1941 Roosevelt Franklin D. Democratic
#>  1945-Roosevelt   275    633        28 1945 Roosevelt Franklin D. Democratic
#> 
```
