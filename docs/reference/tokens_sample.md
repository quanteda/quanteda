# Randomly sample documents from a tokens object

Take a random sample of documents of the specified size from a corpus,
with or without replacement, optionally by grouping variables or with
probability weights.

## Usage

``` r
tokens_sample(
  x,
  size = NULL,
  replace = FALSE,
  prob = NULL,
  by = NULL,
  env = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md) object whose
  documents will be sampled

- size:

  a positive number, the number of documents to select; when used with
  `by`, the number to select from each group or a vector equal in length
  to the number of groups defining the samples to be chosen in each
  category of `by`. By defining a size larger than the number of
  documents, it is possible to oversample when `replace = TRUE`.

- replace:

  if `TRUE`, sample with replacement

- prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled. May not be applied when `by` is used.

- by:

  optional grouping variable for sampling. This will be evaluated in the
  docvars data.frame, so that docvars may be referred to by name without
  quoting. This also changes previous behaviours for `by`. See
  `news(Version >= "2.9", package = "quanteda")` for details.

- env:

  an environment or a list object in which `x` is searched. Passed to
  [substitute](https://rdrr.io/r/base/substitute.html) for non-standard
  evaluation.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

a [tokens](https://quanteda.io/reference/tokens.md) object (re)sampled
on the documents, containing the document variables for the documents
sampled.

## See also

[sample](https://rdrr.io/r/base/sample.html)

## Examples

``` r
set.seed(123)
toks <- tokens(data_corpus_inaugural[1:6])
toks
#> Tokens consisting of 6 documents and 4 docvars.
#> 1789-Washington :
#>  [1] "Fellow-Citizens" "of"              "the"             "Senate"         
#>  [5] "and"             "of"              "the"             "House"          
#>  [9] "of"              "Representatives" ":"               "Among"          
#> [ ... and 1,525 more ]
#> 
#> 1793-Washington :
#>  [1] "Fellow"   "citizens" ","        "I"        "am"       "again"   
#>  [7] "called"   "upon"     "by"       "the"      "voice"    "of"      
#> [ ... and 135 more ]
#> 
#> 1797-Adams :
#>  [1] "When"      "it"        "was"       "first"     "perceived" ","        
#>  [7] "in"        "early"     "times"     ","         "that"      "no"       
#> [ ... and 2,565 more ]
#> 
#> 1801-Jefferson :
#>  [1] "Friends"   "and"       "Fellow"    "Citizens"  ":"         "Called"   
#>  [7] "upon"      "to"        "undertake" "the"       "duties"    "of"       
#> [ ... and 1,911 more ]
#> 
#> 1805-Jefferson :
#>  [1] "Proceeding"    ","             "fellow"        "citizens"     
#>  [5] ","             "to"            "that"          "qualification"
#>  [9] "which"         "the"           "Constitution"  "requires"     
#> [ ... and 2,368 more ]
#> 
#> 1809-Madison :
#>  [1] "Unwilling" "to"        "depart"    "from"      "examples"  "of"       
#>  [7] "the"       "most"      "revered"   "authority" ","         "I"        
#> [ ... and 1,249 more ]
#> 
tokens_sample(toks)
#> Tokens consisting of 6 documents and 4 docvars.
#> 1797-Adams :
#>  [1] "When"      "it"        "was"       "first"     "perceived" ","        
#>  [7] "in"        "early"     "times"     ","         "that"      "no"       
#> [ ... and 2,565 more ]
#> 
#> 1809-Madison :
#>  [1] "Unwilling" "to"        "depart"    "from"      "examples"  "of"       
#>  [7] "the"       "most"      "revered"   "authority" ","         "I"        
#> [ ... and 1,249 more ]
#> 
#> 1793-Washington :
#>  [1] "Fellow"   "citizens" ","        "I"        "am"       "again"   
#>  [7] "called"   "upon"     "by"       "the"      "voice"    "of"      
#> [ ... and 135 more ]
#> 
#> 1801-Jefferson :
#>  [1] "Friends"   "and"       "Fellow"    "Citizens"  ":"         "Called"   
#>  [7] "upon"      "to"        "undertake" "the"       "duties"    "of"       
#> [ ... and 1,911 more ]
#> 
#> 1805-Jefferson :
#>  [1] "Proceeding"    ","             "fellow"        "citizens"     
#>  [5] ","             "to"            "that"          "qualification"
#>  [9] "which"         "the"           "Constitution"  "requires"     
#> [ ... and 2,368 more ]
#> 
#> 1789-Washington :
#>  [1] "Fellow-Citizens" "of"              "the"             "Senate"         
#>  [5] "and"             "of"              "the"             "House"          
#>  [9] "of"              "Representatives" ":"               "Among"          
#> [ ... and 1,525 more ]
#> 
tokens_sample(toks, replace = TRUE) |> docnames()
#> [1] "1805-Jefferson.1"  "1801-Jefferson.1"  "1809-Madison.1"   
#> [4] "1809-Madison.2"    "1789-Washington.1" "1793-Washington.1"
tokens_sample(toks, size = 3, replace = TRUE) |> docnames()
#> [1] "1797-Adams.1"     "1805-Jefferson.1" "1797-Adams.2"    

# sampling using by
docvars(toks)
#>   Year  President FirstName                 Party
#> 1 1789 Washington    George                  none
#> 2 1793 Washington    George                  none
#> 3 1797      Adams      John            Federalist
#> 4 1801  Jefferson    Thomas Democratic-Republican
#> 5 1805  Jefferson    Thomas Democratic-Republican
#> 6 1809    Madison     James Democratic-Republican
tokens_sample(toks, size = 2, replace = TRUE, by = Party) |> docnames()
#> [1] "1809-Madison.1"    "1801-Jefferson.1"  "1797-Adams.1"     
#> [4] "1797-Adams.2"      "1789-Washington.1" "1789-Washington.2"
```
