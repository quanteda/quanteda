# Validate input vectors

Check the range of values and the length of input vectors before used in
control flow or passed to C++ functions.

## Usage

``` r
check_integer(
  x,
  min_len = 1,
  max_len = 1,
  min = -Inf,
  max = Inf,
  strict = FALSE,
  allow_null = FALSE
)

check_double(
  x,
  min_len = 1,
  max_len = 1,
  min = -Inf,
  max = Inf,
  strict = FALSE,
  allow_null = FALSE
)

check_logical(
  x,
  min_len = 1,
  max_len = 1,
  strict = FALSE,
  allow_null = FALSE,
  allow_na = FALSE
)

check_character(
  x,
  min_len = 1,
  max_len = 1,
  min_nchar = 0,
  max_nchar = Inf,
  strict = FALSE,
  allow_null = FALSE,
  normalize = FALSE
)
```

## Arguments

- min_len:

  minimum length of the vector.

- max_len:

  maximum length of the vector.

- min:

  minimum value in the vector.

- max:

  maximum value in the vector.

- strict:

  raise error when `x` is a different type.

- allow_null:

  if `TRUE`, returns `NULL` when `is.null(x)`.

- allow_na:

  if `TRUE`, convert `NA` to `FALSE`

- min_nchar:

  minimum character length of values in the vector.

- max_nchar:

  maximum character length of values in the vector.

- normalize:

  if `TRUE`, normalize Unicode characters by applying
  [stringi](https://rdrr.io/pkg/stringi/man/stri_trans_nf.html).

## Details

Note that value checks are performed after coercion to expected input
types.

## Examples

``` r
if (FALSE) { # \dontrun{
check_integer(0, min = 1) # error
check_integer(-0.1, min = 0) # return 0
check_double(-0.1, min = 0) # error
check_double(numeric(), min_len = 0) # return numeric()
check_double("1.1", min = 1) # returns 1.1
check_double("1.1", min = 1, strict = TRUE) # error
check_double("xyz", min = 1) # error
check_logical(c(TRUE, FALSE), min_len = 3) # error
check_character("_", min_nchar = 1) # return "_"
check_character("", min_nchar = 1) # error
} # }
```
