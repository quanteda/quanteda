# Trim tokens using frequency threshold-based feature selection

Returns a tokens object reduced in size based on document and term
frequency, usually in terms of a minimum frequency, but may also be in
terms of maximum frequencies. Setting a combination of minimum and
maximum frequencies will select features based on a range.

## Usage

``` r
tokens_trim(
  x,
  min_termfreq = NULL,
  max_termfreq = NULL,
  termfreq_type = c("count", "prop", "rank", "quantile"),
  min_docfreq = NULL,
  max_docfreq = NULL,
  docfreq_type = c("count", "prop", "rank", "quantile"),
  max_n = NULL,
  padding = FALSE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md) object

- min_termfreq, max_termfreq:

  minimum/maximum values of feature frequencies across all documents,
  below/above which features will be removed

- termfreq_type:

  how `min_termfreq` and `max_termfreq` are interpreted. `"count"` sums
  the frequencies; `"prop"` divides the term frequencies by the total
  sum; `"rank"` is matched against the inverted ranking of features in
  terms of overall frequency, so that 1, 2, ... are the highest and
  second highest frequency features, and so on; `"quantile"` sets the
  cutoffs according to the quantiles (see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html)) of term
  frequencies.

- min_docfreq, max_docfreq:

  minimum/maximum values of a feature's document frequency, below/above
  which features will be removed

- docfreq_type:

  specify how `min_docfreq` and `max_docfreq` are interpreted. `"count"`
  is the same as `[docfreq](x, scheme = "count")`; `"prop"` divides the
  document frequencies by the total sum; `"rank"` is matched against the
  inverted ranking of document frequency, so that 1, 2, ... are the
  features with the highest and second highest document frequencies, and
  so on; `"quantile"` sets the cutoffs according to the quantiles (see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html)) of document
  frequencies.

- max_n:

  the maximum number of features. Features are selected based on their
  frequencies, but the earlier elements in
  [featnames](https://quanteda.io/reference/featnames.md) or
  [types](https://quanteda.io/reference/types.md) are chosen when the
  frequencies are equal.

- padding:

  if `TRUE`, leave an empty string where the removed tokens previously
  existed.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

A [tokens](https://quanteda.io/reference/tokens.md) object with reduced
size.

## See also

[`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md)

## Examples

``` r
toks <- tokens(data_corpus_inaugural)

# keep only words occurring >= 10 times and in >= 2 documents
tokens_trim(toks, min_termfreq = 10, min_docfreq = 2, padding = TRUE)
#> Tokens consisting of 60 documents and 4 docvars.
#> 1789-Washington :
#>  [1] ""       "of"     "the"    "Senate" "and"    "of"     "the"    ""      
#>  [9] "of"     ""       ":"      ""      
#> [ ... and 1,525 more ]
#> 
#> 1793-Washington :
#>  [1] "Fellow"   "citizens" ","        "I"        "am"       "again"   
#>  [7] "called"   "upon"     "by"       "the"      "voice"    "of"      
#> [ ... and 135 more ]
#> 
#> 1797-Adams :
#>  [1] "When"  "it"    "was"   "first" ""      ","     "in"    "early" "times"
#> [10] ","     "that"  "no"   
#> [ ... and 2,565 more ]
#> 
#> 1801-Jefferson :
#>  [1] ""       "and"    "Fellow" ""       ":"      ""       "upon"   "to"    
#>  [9] ""       "the"    "duties" "of"    
#> [ ... and 1,911 more ]
#> 
#> 1805-Jefferson :
#>  [1] ""             ","            "fellow"       "citizens"     ","           
#>  [6] "to"           "that"         ""             "which"        "the"         
#> [11] "Constitution" "requires"    
#> [ ... and 2,368 more ]
#> 
#> 1809-Madison :
#>  [1] ""          "to"        ""          "from"      "examples"  "of"       
#>  [7] "the"       "most"      ""          "authority" ","         "I"        
#> [ ... and 1,249 more ]
#> 
#> [ reached max_ndoc ... 54 more documents ]

# keep only words occurring >= 10 times and no more than 90% of the documents
tokens_trim(toks, min_termfreq = 10, max_docfreq = 0.9, docfreq_type = "prop",
            padding = TRUE)
#> Tokens consisting of 60 documents and 4 docvars.
#> 1789-Washington :
#>  [1] ""       ""       ""       "Senate" ""       ""       ""       ""      
#>  [9] ""       ""       ":"      ""      
#> [ ... and 1,525 more ]
#> 
#> 1793-Washington :
#>  [1] "Fellow"   "citizens" ""         ""         "am"       "again"   
#>  [7] "called"   "upon"     ""         ""         "voice"    ""        
#> [ ... and 135 more ]
#> 
#> 1797-Adams :
#>  [1] "When"  ""      "was"   "first" ""      ""      ""      "early" "times"
#> [10] ""      ""      ""     
#> [ ... and 2,565 more ]
#> 
#> 1801-Jefferson :
#>  [1] ""       ""       "Fellow" ""       ":"      ""       "upon"   ""      
#>  [9] ""       ""       "duties" ""      
#> [ ... and 1,911 more ]
#> 
#> 1805-Jefferson :
#>  [1] ""             ""             "fellow"       "citizens"     ""            
#>  [6] ""             ""             ""             ""             ""            
#> [11] "Constitution" "requires"    
#> [ ... and 2,368 more ]
#> 
#> 1809-Madison :
#>  [1] ""          ""          ""          ""          "examples"  ""         
#>  [7] ""          "most"      ""          "authority" ""          ""         
#> [ ... and 1,249 more ]
#> 
#> [ reached max_ndoc ... 54 more documents ]
```
