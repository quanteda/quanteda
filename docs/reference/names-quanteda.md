# Special handling for names of quanteda objects

Keeps the element names and rownames in sync with the system docvar
`docname_`.

## Usage

``` r
# S3 method for class 'corpus'
names(x) <- value

# S3 method for class 'tokens'
names(x) <- value

# S4 method for class 'dfm'
rownames(x) <- value

# S4 method for class 'fcm'
rownames(x) <- value
```

## Arguments

- x:

  an R object.

- value:

  a character vector of up to the same length as `x`, or `NULL`.
