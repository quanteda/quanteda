# Flatten a hierarchical dictionary into a list of character vectors

Converts a hierarchical dictionary (a named list of named lists, ending
in character vectors at the lowest level) into a flat list of character
vectors.

## Usage

``` r
flatten_dictionary(dictionary, levels = 1:100)
```

## Arguments

- dictionary:

  a [dictionary](https://quanteda.io/reference/dictionary.md)-class
  object to be flattened

- levels:

  an integer vector indicating levels in the dictionary

## Value

A named list of character vectors

## Examples

``` r
dict1 <- dictionary(
    list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
                    "corrupt*", "propagand", "politici*", "*deceit*",
                    "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
                    "dishonest*", "establishm*", "ruling*"))
     )
flatten_dictionary(dict1)
#> Dictionary object with 1 key entry.
#> - [populism]:
#>   - elit*, consensus*, undemocratic*, referend*, corrupt*, propagand, politici*,
#>     *deceit*, *deceiv*, *betray*, shame*, scandal*, truth*, dishonest*,
#>     establishm*, ruling*

dict2 <- dictionary(
    list(level1a = list(level1a1 = c("l1a11", "l1a12"),
         level1a2 = c("l1a21", "l1a22")),
         level1b = list(level1b1 = c("l1b11", "l1b12"),
         level1b2 = c("l1b21", "l1b22", "l1b23")),
         level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
         level1c1b = list(level1c1b1 = c("lowestalone"))))
     )
flatten_dictionary(dict2)
#> Dictionary object with 6 key entries.
#> - [level1a.level1a1]:
#>   - l1a11, l1a12
#> - [level1a.level1a2]:
#>   - l1a21, l1a22
#> - [level1b.level1b1]:
#>   - l1b11, l1b12
#> - [level1b.level1b2]:
#>   - l1b21, l1b22, l1b23
#> - [level1c.level1c1a.level1c1a1]:
#>   - lowest1, lowest2
#> - [level1c.level1c1b.level1c1b1]:
#>   - lowestalone
flatten_dictionary(dict2, 2)
#> Dictionary object with 6 key entries.
#> - [level1a1]:
#>   - l1a11, l1a12
#> - [level1a2]:
#>   - l1a21, l1a22
#> - [level1b1]:
#>   - l1b11, l1b12
#> - [level1b2]:
#>   - l1b21, l1b22, l1b23
#> - [level1c1a]:
#>   - lowest1, lowest2
#> - [level1c1b]:
#>   - lowestalone
flatten_dictionary(dict2, 1:2)
#> Dictionary object with 6 key entries.
#> - [level1a.level1a1]:
#>   - l1a11, l1a12
#> - [level1a.level1a2]:
#>   - l1a21, l1a22
#> - [level1b.level1b1]:
#>   - l1b11, l1b12
#> - [level1b.level1b2]:
#>   - l1b21, l1b22, l1b23
#> - [level1c.level1c1a]:
#>   - lowest1, lowest2
#> - [level1c.level1c1b]:
#>   - lowestalone
```
