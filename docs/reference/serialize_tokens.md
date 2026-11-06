# Function to serialize list-of-character tokens

Creates a serialized object of tokens, called by
[`tokens()`](https://quanteda.io/reference/tokens.md).

## Usage

``` r
serialize_tokens(x, types_reserved = NULL, ...)
```

## Arguments

- x:

  a list of character vectors

- types_reserved:

  optional pre-existing types for mapping of tokens

- ...:

  additional arguments

## Value

a list the serialized tokens found in each text
