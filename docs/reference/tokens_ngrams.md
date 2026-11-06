# Create n-grams and skip-grams from tokens

Create a set of n-grams (tokens in sequence) from already tokenized text
objects, with an optional skip argument to form skip-grams. Both the
n-gram length and the skip lengths take vectors of arguments to form
multiple lengths or skips in one pass. Implemented in C++ for
efficiency.

## Usage

``` r
tokens_ngrams(
  x,
  n = 2L,
  skip = 0L,
  concatenator = concat(x),
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)

char_ngrams(x, n = 2L, skip = 0L, concatenator = "_")

tokens_skipgrams(
  x,
  n,
  skip,
  concatenator = concat(x),
  apply_if = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a tokens object, or a character vector, or a list of characters

- n:

  integer vector specifying the number of elements to be concatenated in
  each n-gram. Each element of this vector will define a \\n\\ in the
  \\n\\-gram(s) that are produced.

- skip:

  integer vector specifying the adjacency skip size for tokens forming
  the n-grams, default is 0 for only immediately neighbouring words. For
  `skipgrams`, `skip` can be a vector of integers, as the "classic"
  approach to forming skip-grams is to set skip = \\k\\ where \\k\\ is
  the distance for which \\k\\ or fewer skips are used to construct the
  \\n\\-gram. Thus a "4-skip-n-gram" defined as `skip = 0:4` produces
  results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0 skips
  (where 0 skips are typical n-grams formed from adjacent words). See
  Guthrie et al (2006).

- concatenator:

  character for combining words, default is `_` (underscore) character

- apply_if:

  logical vector of length `ndoc(x)`; documents are modified only when
  corresponding values are `TRUE`, others are left unchanged.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

a tokens object consisting a list of character vectors of n-grams, one
list element per text, or a character vector if called on a simple
character vector

## Details

Normally, these functions will be called through
`[tokens](x, ngrams = , ...)`, but these functions are provided in case
a user wants to perform lower-level n-gram construction on tokenized
texts.

`tokens_skipgrams()` is a wrapper to `tokens_ngrams()` that requires
arguments to be supplied for both `n` and `skip`. For \\k\\-skip
skip-grams, set `skip` to `0:`\\k\\, in order to conform to the
definition of skip-grams found in Guthrie et al (2006): A \\k\\
skip-gram is an n-gram which is a superset of all n-grams and each
\\(k-i)\\ skip-gram until \\(k-i)==0\\ (which includes 0 skip-grams).

## Note

`char_ngrams` is a convenience wrapper for a (non-list) vector of
characters, so named to be consistent with quanteda's naming scheme.

## References

Guthrie, David, Ben Allison, Wei Liu, Louise Guthrie, and Yorick Wilks.
2006. "A Closer Look at Skip-Gram Modelling."
`https://aclanthology.org/L06-1210/`

## Examples

``` r
# ngrams
tokens_ngrams(tokens(c("a b c d e", "c d e f g")), n = 2:3)
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "a_b"   "b_c"   "c_d"   "d_e"   "a_b_c" "b_c_d" "c_d_e"
#> 
#> text2 :
#> [1] "c_d"   "d_e"   "e_f"   "f_g"   "c_d_e" "d_e_f" "e_f_g"
#> 

toks <- tokens(c(text1 = "the quick brown fox jumped over the lazy dog"))
tokens_ngrams(toks, n = 1:3)
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "the"         "quick"       "brown"       "fox"         "jumped"     
#>  [6] "over"        "the"         "lazy"        "dog"         "the_quick"  
#> [11] "quick_brown" "brown_fox"  
#> [ ... and 12 more ]
#> 
tokens_ngrams(toks, n = c(2,4), concatenator = " ")
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "the quick"              "quick brown"            "brown fox"             
#>  [4] "fox jumped"             "jumped over"            "over the"              
#>  [7] "the lazy"               "lazy dog"               "the quick brown fox"   
#> [10] "quick brown fox jumped" "brown fox jumped over"  "fox jumped over the"   
#> [ ... and 2 more ]
#> 
tokens_ngrams(toks, n = c(2,4), skip = 1, concatenator = " ")
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "the brown"            "quick fox"            "brown jumped"        
#>  [4] "fox over"             "jumped the"           "over lazy"           
#>  [7] "the dog"              "the brown jumped the" "quick fox over lazy" 
#> [10] "brown jumped the dog"
#> 
# skipgrams
toks <- tokens("insurgents killed in ongoing fighting")
tokens_skipgrams(toks, n = 2, skip = 0:1, concatenator = " ")
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "insurgents killed" "insurgents in"     "killed in"        
#> [4] "killed ongoing"    "in ongoing"        "in fighting"      
#> [7] "ongoing fighting" 
#> 
tokens_skipgrams(toks, n = 2, skip = 0:2, concatenator = " ")
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "insurgents killed"  "insurgents in"      "insurgents ongoing"
#> [4] "killed in"          "killed ongoing"     "killed fighting"   
#> [7] "in ongoing"         "in fighting"        "ongoing fighting"  
#> 
tokens_skipgrams(toks, n = 3, skip = 0:2, concatenator = " ")
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "insurgents killed in"        "insurgents killed ongoing"  
#>  [3] "insurgents killed fighting"  "insurgents in ongoing"      
#>  [5] "insurgents in fighting"      "insurgents ongoing fighting"
#>  [7] "killed in ongoing"           "killed in fighting"         
#>  [9] "killed ongoing fighting"     "in ongoing fighting"        
#> 
```
