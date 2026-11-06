# Utility function to generate a nested list

Utility function to generate a nested list

## Usage

``` r
nest_dictionary(dict, depth)
```

## Arguments

- dict:

  a flat dictionary

- depth:

  depths of nested element

## Examples

``` r
lis <- list("A" = c("a", "aa", "aaa"), "B" = c("b", "bb"), "C" = c("c", "cc"), "D" = c("ddd"))
dict <- quanteda:::list2dictionary(lis)
quanteda:::nest_dictionary(dict, c(1, 1, 2, 2))
#> $A
#> $A[[1]]
#> [1] "a"   "aa"  "aaa"
#> 
#> 
#> $B
#> $B[[1]]
#> [1] "b"  "bb"
#> 
#> $B$C
#> $B$C[[1]]
#> [1] "c"  "cc"
#> 
#> 
#> $B$D
#> $B$D[[1]]
#> [1] "ddd"
#> 
#> 
#> 
quanteda:::nest_dictionary(dict, c(1, 2, 1, 2))
#> $A
#> $A[[1]]
#> [1] "a"   "aa"  "aaa"
#> 
#> $A$B
#> $A$B[[1]]
#> [1] "b"  "bb"
#> 
#> 
#> 
#> $C
#> $C[[1]]
#> [1] "c"  "cc"
#> 
#> $C$D
#> $C$D[[1]]
#> [1] "ddd"
#> 
#> 
#> 
```
