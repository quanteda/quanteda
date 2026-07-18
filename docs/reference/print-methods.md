# Print methods for quanteda core objects

Print method for quanteda objects. In each `max_n*` option, 0 shows
none, and -1 shows all.

## Usage

``` r
# S3 method for class 'corpus'
print(
  x,
  max_ndoc = quanteda_options("print_corpus_max_ndoc"),
  max_nchar = quanteda_options("print_corpus_max_nchar"),
  show_summary = quanteda_options("print_corpus_summary"),
  ...
)

# S4 method for class 'dfm'
print(
  x,
  max_ndoc = quanteda_options("print_dfm_max_ndoc"),
  max_nfeat = quanteda_options("print_dfm_max_nfeat"),
  show_summary = quanteda_options("print_dfm_summary"),
  ...
)

# S4 method for class 'dictionary2'
print(
  x,
  max_nkey = quanteda_options("print_dictionary_max_nkey"),
  max_nval = quanteda_options("print_dictionary_max_nval"),
  show_summary = quanteda_options("print_dictionary_summary"),
  ...
)

# S4 method for class 'fcm'
print(
  x,
  max_nfeat = quanteda_options("print_dfm_max_nfeat"),
  show_summary = TRUE,
  ...
)

# S3 method for class 'kwic'
print(
  x,
  max_nrow = quanteda_options("print_kwic_max_nrow"),
  show_summary = quanteda_options("print_kwic_summary"),
  ...
)

# S3 method for class 'tokens'
print(
  x,
  max_ndoc = quanteda_options("print_tokens_max_ndoc"),
  max_ntoken = quanteda_options("print_tokens_max_ntoken"),
  show_summary = quanteda_options("print_tokens_summary"),
  ...
)
```

## Arguments

- x:

  the object to be printed

- max_ndoc:

  max number of documents to print; default is from the
  `print_*_max_ndoc` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- max_nchar:

  max number of tokens to print; default is from the
  `print_corpus_max_nchar` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- show_summary:

  print a brief summary indicating the number of documents and other
  characteristics of the object, such as docvars or sparsity.

- ...:

  passed to [`base::print()`](https://rdrr.io/r/base/print.html) for
  [tokens](https://quanteda.io/reference/tokens.md); unused for all
  other objects.

- max_nfeat:

  max number of features to print; default is from the
  `print_dfm_max_nfeat` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- max_nkey:

  max number of keys to print; default is from the
  `print_dictionary_max_max_nkey` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- max_nval:

  max number of values to print; default is from the
  `print_dictionary_max_nval` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- max_nrow:

  max number of documents to print; default is from the
  `print_kwic_max_nrow` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

- max_ntoken:

  max number of tokens to print; default is from the
  `print_tokens_max_ntoken` setting of
  [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md).

## See also

[`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)

## Examples

``` r
corp <- corpus(data_char_ukimmig2010)
print(corp, max_ndoc = 3, max_nchar = 40)
#> Corpus consisting of 9 documents.
#> BNP :
#> "IMMIGRATION: AN UNPARALLELED CRISIS WHIC..."
#> 
#> Coalition :
#> "IMMIGRATION. The Government believes th..."
#> 
#> Conservative :
#> "Attract the brightest and best to our co..."
#> 
#> [ reached max_ndoc ... 6 more documents ]

toks <- tokens(corp)
print(toks, max_ndoc = 3, max_ntoken = 6)
#> Tokens consisting of 9 documents.
#> BNP :
#> [1] "IMMIGRATION"  ":"            "AN"           "UNPARALLELED" "CRISIS"      
#> [6] "WHICH"       
#> [ ... and 3,274 more ]
#> 
#> Coalition :
#> [1] "IMMIGRATION" "."           "The"         "Government"  "believes"   
#> [6] "that"       
#> [ ... and 254 more ]
#> 
#> Conservative :
#> [1] "Attract"   "the"       "brightest" "and"       "best"      "to"       
#> [ ... and 493 more ]
#> 
#> [ reached max_ndoc ... 6 more documents ]

dfmat <- dfm(toks)
print(dfmat, max_ndoc = 3, max_nfeat = 10)
#> Document-feature matrix of: 9 documents, 1,656 features (81.73% sparse) and 0
#> docvars.
#>               features
#> docs           immigration  : an unparalleled crisis which only the bnp can
#>   BNP                   21 12 15            1      2     9    4 191  13   1
#>   Coalition              6  0  1            0      0     0    0  13   0   0
#>   Conservative           3  2  2            0      0     0    1  21   0   2
#> [ reached max_ndoc ... 6 more documents, reached max_nfeat ... 1,646 more
#> features ]
```
