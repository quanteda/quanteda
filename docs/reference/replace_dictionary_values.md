# Internal function to replace dictionary values

Internal function to replace dictionary values

## Usage

``` r
replace_dictionary_values(dict, from, to)
```

## Arguments

- dict:

  a [dictionary](https://quanteda.io/reference/dictionary.md) object

## Examples

``` r
dict <- list(KEY1 = list(SUBKEY1 = list("A_B"),
                          SUBKEY2 = list("C_D")),
              KEY2 = list(SUBKEY3 = list("E_F"),
                          SUBKEY4 = list("G_F_I")),
              KEY3 = list(SUBKEY5 = list(SUBKEY7 = list("J_K")),
                          SUBKEY6 = list(SUBKEY8 = list("L"))))
quanteda:::replace_dictionary_values(dict, "_", " ")
#> $KEY1
#> $KEY1$SUBKEY1
#> $KEY1$SUBKEY1[[1]]
#> [1] "A B"
#> 
#> 
#> $KEY1$SUBKEY2
#> $KEY1$SUBKEY2[[1]]
#> [1] "C D"
#> 
#> 
#> 
#> $KEY2
#> $KEY2$SUBKEY3
#> $KEY2$SUBKEY3[[1]]
#> [1] "E F"
#> 
#> 
#> $KEY2$SUBKEY4
#> $KEY2$SUBKEY4[[1]]
#> [1] "G F I"
#> 
#> 
#> 
#> $KEY3
#> $KEY3$SUBKEY5
#> $KEY3$SUBKEY5$SUBKEY7
#> $KEY3$SUBKEY5$SUBKEY7[[1]]
#> [1] "J K"
#> 
#> 
#> 
#> $KEY3$SUBKEY6
#> $KEY3$SUBKEY6$SUBKEY8
#> $KEY3$SUBKEY6$SUBKEY8[[1]]
#> [1] "L"
#> 
#> 
#> 
#> 
```
