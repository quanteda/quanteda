# Check object class for functions

Checks if the method is defined for the class.

## Usage

``` r
check_class(class, method, defunct_methods = NULL)
```

## Arguments

- class:

  the object class to check

- method:

  the name of functions to be called

## Examples

``` r
if (FALSE) { # \dontrun{
quanteda:::check_class("tokens", "dfm_select")
} # }
```
