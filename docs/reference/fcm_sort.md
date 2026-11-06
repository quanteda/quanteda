# Sort an fcm in alphabetical order of the features

Sorts an [fcm](https://quanteda.io/reference/fcm.md) in alphabetical
order of the features.

## Usage

``` r
fcm_sort(x)
```

## Arguments

- x:

  [fcm](https://quanteda.io/reference/fcm.md) object

## Value

A [fcm](https://quanteda.io/reference/fcm.md) object whose features have
been alphabetically sorted. Differs from `fcm_sort()` in that this
function sorts the fcm by the feature labels, not the counts of the
features.

## Author

Kenneth Benoit

## Examples

``` r
# with tri = FALSE
fcmat1 <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = FALSE)
rownames(fcmat1)[3] <- colnames(fcmat1)[3] <- "Z"
fcmat1
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features A X Z C B
#>        A 1 3 3 3 4
#>        X 3 0 2 2 3
#>        Z 3 2 0 2 3
#>        C 3 2 2 0 3
#>        B 4 3 3 3 1
fcm_sort(fcmat1)
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features A B C X Z
#>        A 1 4 3 3 3
#>        B 4 1 3 3 3
#>        C 3 3 0 2 2
#>        X 3 3 2 0 2
#>        Z 3 3 2 2 0

# with tri = TRUE
fcmat2 <- fcm(tokens(c("A X Y C B A", "X Y C A B B")), tri = TRUE)
rownames(fcmat2)[3] <- colnames(fcmat2)[3] <- "Z"
fcmat2
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features A X Z C B
#>        A 1 3 3 3 4
#>        X 0 0 2 2 3
#>        Z 0 0 0 2 3
#>        C 0 0 0 0 3
#>        B 0 0 0 0 1
fcm_sort(fcmat2)
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features A B C X Z
#>        A 1 4 3 3 3
#>        B 0 1 3 3 3
#>        C 0 0 0 2 2
#>        X 0 0 0 0 2
#>        Z 0 0 0 0 0
```
