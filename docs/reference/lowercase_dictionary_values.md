# Internal function to lowercase dictionary values

Internal function to lowercase dictionary values

## Usage

``` r
lowercase_dictionary_values(dict)
```

## Arguments

- dict:

  a dictionary object

## Examples

``` r
dict <- list(KEY1 = list(SUBKEY1 = c("A", "B"),
                          SUBKEY2 = c("C", "D")),
              KEY2 = list(SUBKEY3 = c("E", "F"),
                          SUBKEY4 = c("G", "F", "I")),
              KEY3 = list(SUBKEY5 = list(SUBKEY7 = c("J", "K")),
                          SUBKEY6 = list(SUBKEY8 = c("L"))))
quanteda:::lowercase_dictionary_values(dict)
#> $KEY1
#> $KEY1$SUBKEY1
#> [1] "a" "b"
#> 
#> $KEY1$SUBKEY2
#> [1] "c" "d"
#> 
#> 
#> $KEY2
#> $KEY2$SUBKEY3
#> [1] "e" "f"
#> 
#> $KEY2$SUBKEY4
#> [1] "g" "f" "i"
#> 
#> 
#> $KEY3
#> $KEY3$SUBKEY5
#> $KEY3$SUBKEY5$SUBKEY7
#> [1] "j" "k"
#> 
#> 
#> $KEY3$SUBKEY6
#> $KEY3$SUBKEY6$SUBKEY8
#> [1] "l"
#> 
#> 
#> 
```
