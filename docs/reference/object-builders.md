# Object builders

Functions to build or re-build core objects, or to upgrade earlier
versions of these objects to the current format.

## Usage

``` r
build_dfm(
  x,
  features,
  docvars = data.frame(),
  meta = list(),
  class = NULL,
  ...
)

rebuild_dfm(x, attrs)

upgrade_dfm(x)

build_tokens(
  x,
  types,
  padding = TRUE,
  docvars = data.frame(),
  meta = list(),
  class = NULL,
  ...
)

rebuild_tokens(x, attrs)

upgrade_tokens(x)

build_corpus(x, docvars = data.frame(), meta = list(), class = NULL, ...)

rebuild_corpus(x, attrs)

upgrade_corpus(x)

build_dictionary2(x, meta = list(), class = "dictionary2", ...)

rebuild_dictionary2(x, attrs)

upgrade_dictionary2(x)

build_fcm(
  x,
  features1,
  features2 = features1,
  meta = list(),
  class = "fcm",
  ...
)

rebuild_fcm(x, attrs)

upgrade_fcm(x)
```

## Arguments

- x:

  an input [corpus](https://quanteda.io/reference/corpus.md),
  [tokens](https://quanteda.io/reference/tokens.md),
  [dfm](https://quanteda.io/reference/dfm.md),
  [fcm](https://quanteda.io/reference/fcm.md) or
  [dictionary](https://quanteda.io/reference/dictionary.md) object.

- features:

  character for feature of resulting `dfm`.

- docvars:

  data.frame for document level variables created by
  [`make_docvars()`](https://quanteda.io/reference/make_docvars.md).
  Names of documents are extracted from the `docname_` column.

- meta:

  list for meta fields

- class:

  class labels to be attached to the object.

- ...:

  values saved in the object meta fields. They overwrite values passed
  via `meta`. If not specified, default values in
  [`make_meta()`](https://quanteda.io/reference/make_meta.md) will be
  used.

- attrs:

  a list of attributes to be reassigned

- types:

  character for types of resulting the `tokens` object.

- padding:

  logical indicating if the `tokens` object contains paddings.

- features1:

  character for row feature of resulting `fcm`.

- features2:

  character for column feature of resulting `fcm` iff. different from
  `feature1`

## Examples

``` r
quanteda:::build_tokens(
    list(c(1, 2, 3), c(4, 5, 6)),
    docvars = quanteda:::make_docvars(n = 2L),
    types = c("a", "b", "c", "d", "e", "f"),
    padding = FALSE
)
#> Tokens consisting of 2 documents.
#> text1 :
#> [1] "a" "b" "c"
#> 
#> text2 :
#> [1] "d" "e" "f"
#> 
quanteda:::build_corpus(
    c("a b c", "d e f"),
    docvars = quanteda:::make_docvars(n = 2L),
    unit = "sentence"
)
#> Corpus consisting of 2 documents.
#> text1 :
#> "a b c"
#> 
#> text2 :
#> "d e f"
#> 
```
