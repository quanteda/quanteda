# Get word types from a tokens object

Get unique types of tokens from a
[tokens](https://quanteda.io/reference/tokens.md) object.

## Usage

``` r
types(x)
```

## Arguments

- x:

  a tokens object

## See also

[featnames](https://quanteda.io/reference/featnames.md)

## Examples

``` r
toks <- tokens(data_corpus_inaugural)
head(types(toks), 20)
#>  [1] "Fellow-Citizens" "of"              "the"             "Senate"         
#>  [5] "and"             "House"           "Representatives" ":"              
#>  [9] "Among"           "vicissitudes"    "incident"        "to"             
#> [13] "life"            "no"              "event"           "could"          
#> [17] "have"            "filled"          "me"              "with"           
```
