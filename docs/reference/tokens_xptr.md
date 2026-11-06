# Methods for tokens_xptr objects

Methods for creating and testing for `tokens_xptr` objects, which are
[tokens](https://quanteda.io/reference/tokens.md) objects containing
pointers to memory locations that can be passed by reference for
efficient processing in `tokens_*()` functions that modify them, or for
constructing a document-feature matrix without requiring a deep copy to
be passed to [`dfm()`](https://quanteda.io/reference/dfm.md).

`is.tokens_xptr()` tests whether an object is of class `tokens_xtpr`.

`as.tokens_xptr()` coerces a `tokens` object to an external
pointer-based tokens object, or returns a deep copy of a `tokens_xtpr`
when `x` is already a `tokens_xtpr` object.

## Usage

``` r
is.tokens_xptr(x)

as.tokens_xptr(x)

# S3 method for class 'tokens'
as.tokens_xptr(x)

# S3 method for class 'tokens_xptr'
as.tokens_xptr(x)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md) object to convert
  or a `tokens_xptr` class object to deep copy.

## Value

`is.tokens_xptr()` returns `TRUE` if the object is a external
pointer-based tokens object, `FALSE` otherwise.

`as.tokens_xptr()` returns a (deep copy of a) `tokens_xtpr` class
object.
