# Segment tokens object by chunks of a given size

Segment tokens into new documents of equally sized token lengths, with
the possibility of overlapping the chunks.

## Usage

``` r
tokens_chunk(
  x,
  size,
  overlap = 0,
  use_docvars = TRUE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  [tokens](https://quanteda.io/reference/tokens.md) object whose token
  elements will be segmented into chunks

- size:

  integer; the token length of the chunks

- overlap:

  integer; the number of tokens in a chunk to be taken from the last
  `overlap` tokens from the preceding chunk

- use_docvars:

  if `TRUE`, repeat the docvar values for each chunk; if `FALSE`, drop
  the docvars in the chunked tokens

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

A [tokens](https://quanteda.io/reference/tokens.md) object whose
documents have been split into chunks of length `size`.

## See also

[`tokens_segment()`](https://quanteda.io/reference/tokens_segment.md)

## Examples

``` r
txts <- c(doc1 = "Fellow citizens, I am again called upon by the voice of
                  my country to execute the functions of its Chief Magistrate.",
          doc2 = "When the occasion proper for it shall arrive, I shall
                  endeavor to express the high sense I entertain of this
                  distinguished honor.")
toks <- tokens(txts)
tokens_chunk(toks, size = 5)
#> Tokens consisting of 10 documents.
#> doc1.1 :
#> [1] "Fellow"   "citizens" ","        "I"        "am"      
#> 
#> doc1.2 :
#> [1] "again"  "called" "upon"   "by"     "the"   
#> 
#> doc1.3 :
#> [1] "voice"   "of"      "my"      "country" "to"     
#> 
#> doc1.4 :
#> [1] "execute"   "the"       "functions" "of"        "its"      
#> 
#> doc1.5 :
#> [1] "Chief"      "Magistrate" "."         
#> 
#> doc2.1 :
#> [1] "When"     "the"      "occasion" "proper"   "for"     
#> 
#> [ reached max_ndoc ... 4 more documents ]
tokens_chunk(toks, size = 5, overlap = 4)
#> Tokens consisting of 47 documents.
#> doc1.1 :
#> [1] "Fellow"   "citizens" ","        "I"        "am"      
#> 
#> doc1.2 :
#> [1] "citizens" ","        "I"        "am"       "again"   
#> 
#> doc1.3 :
#> [1] ","      "I"      "am"     "again"  "called"
#> 
#> doc1.4 :
#> [1] "I"      "am"     "again"  "called" "upon"  
#> 
#> doc1.5 :
#> [1] "am"     "again"  "called" "upon"   "by"    
#> 
#> doc1.6 :
#> [1] "again"  "called" "upon"   "by"     "the"   
#> 
#> [ reached max_ndoc ... 41 more documents ]
```
