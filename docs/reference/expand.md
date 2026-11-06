# Simpler and faster version of expand.grid() in base package

Simpler and faster version of expand.grid() in base package

## Usage

``` r
expand(elem)
```

## Arguments

- elem:

  list of elements to be combined

## Examples

``` r
quanteda:::expand(list(c("a", "b", "c"), c("x", "y")))
#> [[1]]
#> [1] "a" "x"
#> 
#> [[2]]
#> [1] "b" "x"
#> 
#> [[3]]
#> [1] "c" "x"
#> 
#> [[4]]
#> [1] "a" "y"
#> 
#> [[5]]
#> [1] "b" "y"
#> 
#> [[6]]
#> [1] "c" "y"
#> 
```
