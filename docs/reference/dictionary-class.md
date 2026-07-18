# dictionary class objects and functions

The `dictionary2` class constructed by
[`dictionary()`](https://quanteda.io/reference/dictionary.md), and
associated core class functions.

## Usage

``` r
# S4 method for class 'dictionary2'
as.list(x, flatten = FALSE, levels = 1:100)

# S4 method for class 'dictionary2,index,ANY,ANY'
x[i]

# S4 method for class 'dictionary2,index'
x[[i]]

# S3 method for class 'dictionary2'
x$name

# S4 method for class 'dictionary2'
c(x, ...)
```

## Arguments

- flatten:

  flatten the nested structure if `TRUE`

- levels:

  integer vector indicating levels in the dictionary. Used only when
  `flatten = TRUE`.

- i:

  index for entries

- name:

  the dictionary key

- ...:

  [dictionary](https://quanteda.io/reference/dictionary.md) objects to
  be concatenated

## Slots

- `.Data`:

  named list of mode character, where each element name is a dictionary
  "key" and each element is one or more dictionary entry "values"
  consisting of a pattern match

- `meta`:

  list of object metadata
