# Internal function to select dictionary nested levels

Internal function to select dictionary nested levels

## Usage

``` r
select_dictionary_levels(dict, levels = 1:100, level = 1)
```

## Arguments

- dict:

  a dictionary object

- levels:

  an integer vector indicating levels to select

- level:

  an internal argument to pass current levels

## Examples

``` r
dict <- list("A" = list("B" = c("b", "B"), c("a", "A", "aa")))
quanteda:::select_dictionary_levels(dict, 1:2)
#> $A
#> $A$B
#> [1] "b" "B"
#> 
#> $A[[2]]
#> [1] "a"  "A"  "aa"
#> 
#> 
quanteda:::select_dictionary_levels(dict, 1)
#> $A
#> $A[[1]]
#> [1] "a"  "A"  "aa"
#> 
#> 
quanteda:::select_dictionary_levels(dict, 2)
#> $B
#> [1] "b" "B"
#> 
```
