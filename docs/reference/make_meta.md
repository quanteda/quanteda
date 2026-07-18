# Internal functions to create a list of the meta fields

Internal functions to create a list of the meta fields

## Usage

``` r
make_meta(class, inherit = NULL, ...)

make_meta_system(inherit = NULL)

make_meta_corpus(inherit = NULL, ...)

make_meta_tokens(inherit = NULL, ...)

make_meta_dfm(inherit = NULL, ...)

make_meta_fcm(inherit = NULL, ...)

make_meta_dictionary2(inherit = NULL, ...)

update_meta(default, inherit, ..., warn = TRUE)
```

## Arguments

- class:

  object class either `dfm`, `tokens` or `corpus`

- inherit:

  list from the meta attribute

- ...:

  values assigned to the object meta fields

- default:

  default values for the meta attribute
