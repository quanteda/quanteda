# Sample a vector

Return a sample from a vector within a grouping variable if specified.

## Usage

``` r
resample(x, size = NULL, replace = FALSE, prob = NULL, by = NULL)
```

## Arguments

- x:

  numeric vector

- size:

  the number of items to sample within each group, as a positive number
  or a vector of numbers equal in length to the number of groups. If
  `NULL`, the sampling is stratified by group in the original group
  sizes.

- replace:

  if `TRUE`, sample with replacement

- prob:

  a vector of probability weights for values in `x`

- by:

  a grouping vector equal in length to `length(x)`

## Value

`x` resampled within groups

## Examples

``` r
set.seed(100)
grvec <- c(rep("a", 3), rep("b", 4), rep("c", 3))
quanteda:::resample(1:10, replace = FALSE, by = grvec)
#>  [1]  2  1  3  7  6  4  5  9 10  8
quanteda:::resample(1:10, replace = TRUE, by = grvec)
#>  [1]  2  2  3  5  5  6  6 10 10  9
quanteda:::resample(1:10, size = 2, replace = TRUE, by = grvec)
#> [1] 1 3 6 7 9 8
quanteda:::resample(1:10, size = c(1, 1, 3), replace = TRUE, by = grvec)
#> [1]  2  6  9  8 10
```
