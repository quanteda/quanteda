# Return the concatenator character from an object

Get the concatenator character from a
[tokens](https://quanteda.io/reference/tokens.md) object.

## Usage

``` r
concat(x)

concatenator(x)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md) object

## Value

a character of length 1

## Details

The concatenator character is a special delimiter used to link separate
tokens in multi-token phrases. It is embedded in the meta-data of tokens
objects and used in downstream operations, such as
[`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
or [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md).
It can be extracted using `concat()` and set using
`tokens(x, concatenator = ...)` when `x` is a tokens object.

The default `_` is recommended since it will not be removed during
normal cleaning and tokenization (while nearly all other punctuation
characters, at least those in the Unicode punctuation class `[P]` will
be removed).

## Examples

``` r
toks <- tokens(data_corpus_inaugural[1:5])
concat(toks)
#> [1] "_"
```
