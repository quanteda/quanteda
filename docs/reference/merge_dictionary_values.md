# Internal function to merge values of duplicated keys

Internal function to merge values of duplicated keys

## Usage

``` r
merge_dictionary_values(dict)
```

## Arguments

- dict:

  a dictionary object

## Examples

``` r
dict <- list("A" = list(AA = list("aaaaa"), "a"),
             "B" = list("b"),
             "C" = list("c"),
             "A" = list("aa"))
quanteda:::merge_dictionary_values(dict)
#> $A
#> $A$AA
#> $A$AA[[1]]
#> [1] "aaaaa"
#> 
#> 
#> $A[[2]]
#> [1] "a"  "aa"
#> 
#> 
#> $B
#> $B[[1]]
#> [1] "b"
#> 
#> 
#> $C
#> $C[[1]]
#> [1] "c"
#> 
#> 
```
