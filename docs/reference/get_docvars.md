# Internal function to extract docvars

Internal function to extract docvars

## Usage

``` r
get_docvars(x, field = NULL, user = TRUE, system = FALSE, drop = FALSE)
```

## Arguments

- x:

  an object from which docvars are extracted

- field:

  name of docvar fields

- user:

  if `TRUE`, return user variables

- system:

  if `TRUE`, return system variables

- drop:

  if `TRUE`, convert data.frame with one variable to a vector
