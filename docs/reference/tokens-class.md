# Base method extensions for tokens objects

Extensions of base R functions for tokens objects.

## Usage

``` r
# S3 method for class 'tokens'
unlist(x, recursive = FALSE, use.names = TRUE)

# S3 method for class 'tokens'
x[i, drop_docid = TRUE]

# S3 method for class 'tokens'
t1 + t2

# S3 method for class 'tokens_xptr'
c(...)

# S3 method for class 'tokens'
c(...)
```

## Arguments

- x:

  a tokens object

- recursive:

  a required argument for [unlist](https://rdrr.io/r/base/unlist.html)
  but inapplicable to [tokens](https://quanteda.io/reference/tokens.md)
  objects.

- i:

  document names or indices for documents to extract.

- drop_docid:

  if `TRUE`, `docid` for documents are removed as the result of
  extraction.

- t1:

  tokens one to be added

- t2:

  tokens two to be added

## Value

`unlist` returns a simple vector of characters from a
[tokens](https://quanteda.io/reference/tokens.md) object.

`c(...)` and `+` return a tokens object whose documents have been added
as a single sequence of documents.

## Examples

``` r
toks <- tokens(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))
str(toks)
#> List of 3
#>  $ d1: chr [1:3] "one" "two" "three"
#>  $ d2: chr [1:3] "four" "five" "six"
#>  $ d3: chr [1:2] "seven" "eight"
#>  - attr(*, "class")= chr "tokens"
#>  - attr(*, "types")= chr [1:8] "one" "two" "three" "four" ...
#>  - attr(*, "padding")= logi TRUE
#>  - attr(*, "docvars")='data.frame':  3 obs. of  3 variables:
#>   ..$ docname_: chr [1:3] "d1" "d2" "d3"
#>   ..$ docid_  : Factor w/ 3 levels "d1","d2","d3": 1 2 3
#>   ..$ segid_  : int [1:3] 1 1 1
#>  - attr(*, "meta")=List of 3
#>   ..$ system:List of 5
#>   .. ..$ package-version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. .. ..$ : int [1:4] 4 5 0 9000
#>   .. ..$ r-version      :Classes 'R_system_version', 'package_version', 'numeric_version'  hidden list of 1
#>   .. .. ..$ : int [1:3] 4 6 0
#>   .. ..$ system         : Named chr [1:3] "Darwin" "arm64" "kbenoit"
#>   .. .. ..- attr(*, "names")= chr [1:3] "sysname" "machine" "user"
#>   .. ..$ directory      : chr "/Users/kbenoit/Dropbox/GitHub/quanteda/quanteda/docs/reference"
#>   .. ..$ created        : Date[1:1], format: "2026-07-23"
#>   ..$ object:List of 7
#>   .. ..$ unit        : chr "documents"
#>   .. ..$ what        : chr "word"
#>   .. ..$ tokenizer   : chr "tokenize_word4"
#>   .. ..$ ngram       : int 1
#>   .. ..$ skip        : int 0
#>   .. ..$ concatenator: chr "_"
#>   .. ..$ summary     :List of 2
#>   .. .. ..$ hash: chr(0) 
#>   .. .. ..$ data: NULL
#>   ..$ user  : list()
toks[c(1,3)]
#> Tokens consisting of 2 documents.
#> d1 :
#> [1] "one"   "two"   "three"
#> 
#> d3 :
#> [1] "seven" "eight"
#> 
# combining tokens
toks1 <- tokens(c(doc1 = "a b c d e", doc2 = "f g h"))
toks2 <- tokens(c(doc3 = "1 2 3"))
toks1 + toks2
#> Tokens consisting of 3 documents.
#> doc1 :
#> [1] "a" "b" "c" "d" "e"
#> 
#> doc2 :
#> [1] "f" "g" "h"
#> 
#> doc3 :
#> [1] "1" "2" "3"
#> 
c(toks1, toks2)
#> Tokens consisting of 3 documents.
#> doc1 :
#> [1] "a" "b" "c" "d" "e"
#> 
#> doc2 :
#> [1] "f" "g" "h"
#> 
#> doc3 :
#> [1] "1" "2" "3"
#> 
```
