# Conditionally format messages

Conditionally format messages

## Usage

``` r
msg(format, ..., prepend = "", append = "")
```

## Arguments

- format:

  character vector of format strings

- ...:

  vectors (coercible to integer, real, or character)

- prepend:

  text is added before the message

- append:

  text is added after the message

## See also

[stringi::stri_sprintf](https://rdrr.io/pkg/stringi/man/stri_sprintf.html)

## Examples

``` r
quanteda:::msg("you cannot delete %s %s", 2000, "documents")
#> [1] "you cannot delete 2,000 documents"
```
