# A paragraph of text for testing various text-based functions

This is a long paragraph (2,914 characters) of text taken from a debate
on Joe Higgins, delivered December 8, 2011.

## Usage

``` r
data(data_char_sampletext)
```

## Format

character vector with one element

## Source

Dáil Éireann Debate, [Financial Resolution No. 13: General
(Resumed).](https://www.oireachtas.ie/en/debates/find/) 7 December 2011.
vol. 749, no. 1.

## Examples

``` r
tokens(data_char_sampletext, remove_punct = TRUE)
#> Tokens consisting of 1 document.
#> text1 :
#>  [1] "Instead"     "we"          "have"        "a"           "Fine"       
#>  [6] "Gael-Labour" "Party"       "Government"  "coming"      "into"       
#> [11] "power"       "promising"  
#> [ ... and 486 more ]
#> 
```
