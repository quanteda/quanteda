# Internal function to flatten a nested list

Internal function to flatten a nested list

## Usage

``` r
flatten_list(
  lis,
  levels = 1:100,
  level = 1,
  key_parent = "",
  lis_flat = list()
)
```

## Arguments

- lis:

  a nested list

- levels:

  an integer vector indicating levels in the list

- level:

  an internal argument to pass current levels

- key_parent:

  an internal argument to pass for parent keys

- lis_flat:

  an internal argument to pass the flattened list

## Examples

``` r
lis <- list("A" = list("B" = c("b", "B"), c("a", "A", "aa")))
quanteda:::flatten_list(lis, 1:2)
#> $A.B
#> [1] "b" "B"
#> 
#> $A
#> [1] "a"  "A"  "aa"
#> 
quanteda:::flatten_list(lis, 1)
#> $A
#> [1] "b"  "B"  "a"  "A"  "aa"
#> 
```
