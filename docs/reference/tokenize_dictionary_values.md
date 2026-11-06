# Internal function to tokenize dictionary values

Internal function to tokenize dictionary values

## Usage

``` r
tokenize_dictionary_values(dict, separator, ...)
```

## Arguments

- dict:

  list of object

## Examples

``` r
dict <- dictionary(list(ASIA = list("IN" = "\u5370\u5ea6",
                                    "ID" = "\u5370\u5ea6\u5c3c\u897f\u4e9a")))
quanteda:::tokenize_dictionary_values(dict, " ")
#> Dictionary object with 1 primary key entry and 2 nested levels.
#> - [ASIA]:
#>   - [IN]:
#>     - 印度
#>   - [ID]:
#>     - 印度 尼西亚
```
