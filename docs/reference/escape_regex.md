# Internal function for `select_types()` to escape regular expressions

This function escapes glob patterns before `utils:glob2rx()`, therefore
\* and ? are unescaped.

## Usage

``` r
escape_regex(x)
```

## Arguments

- x:

  character vector to be escaped
