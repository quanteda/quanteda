# Convert the case of character objects

`char_tolower` and `char_toupper` are replacements for
[base::tolower()](https://rdrr.io/r/base/chartr.html) and
[base::tolower()](https://rdrr.io/r/base/chartr.html) based on the
stringi package. The stringi functions for case conversion are superior
to the base functions because they correctly handle case conversion for
Unicode. In addition, the `*_tolower()` functions provide an option for
preserving acronyms.

## Usage

``` r
char_tolower(x, keep_acronyms = FALSE)

char_toupper(x)
```

## Arguments

- x:

  the input object whose character/tokens/feature elements will be
  case-converted

- keep_acronyms:

  logical; if `TRUE`, do not lowercase any all-uppercase words (applies
  only to `*_tolower()` functions)

## Examples

``` r
txt1 <- c(txt1 = "b A A", txt2 = "C C a b B")
char_tolower(txt1)
#>        txt1        txt2 
#>     "b a a" "c c a b b" 
char_toupper(txt1)
#>        txt1        txt2 
#>     "B A A" "C C A B B" 

# with acronym preservation
txt2 <- c(text1 = "England and France are members of NATO and UNESCO",
          text2 = "NASA sent a rocket into space.")
char_tolower(txt2)
#>                                               text1 
#> "england and france are members of nato and unesco" 
#>                                               text2 
#>                    "nasa sent a rocket into space." 
char_tolower(txt2, keep_acronyms = TRUE)
#>                                               text1 
#> "england and france are members of NATO and UNESCO" 
#>                                               text2 
#>                    "NASA sent a rocket into space." 
char_toupper(txt2)
#>                                               text1 
#> "ENGLAND AND FRANCE ARE MEMBERS OF NATO AND UNESCO" 
#>                                               text2 
#>                    "NASA SENT A ROCKET INTO SPACE." 
```
