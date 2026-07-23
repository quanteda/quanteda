# Coercion and checking methods for corpus objects

Coercion functions to and from
[corpus](https://quanteda.io/reference/corpus.md) objects, including
conversion to a plain [character](https://rdrr.io/r/base/character.html)
object; and checks for whether an object is a corpus.

## Usage

``` r
# S3 method for class 'corpus'
as.character(x, use.names = TRUE, ...)

is.corpus(x)

as.corpus(x)
```

## Arguments

- x:

  object to be coerced or checked

- use.names:

  logical; preserve (document) names if `TRUE`

- ...:

  additional arguments used by specific methods

## Value

[`as.character()`](https://rdrr.io/r/base/character.html) returns the
corpus as a plain character vector, with or without named elements.

`is.corpus` returns `TRUE` if the object is a corpus.

`as.corpus()` upgrades a corpus object to the newest format. object.

## Note

`as.character(x)` where `x` is a corpus is equivalent to calling the
deprecated `texts(x)`.
