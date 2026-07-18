# Shortcut functions to access or assign metadata

Internal functions to access or replace an object metadata field without
going through attribute trees. `field_system()`, `field_object()` and
`field_user()` correspond to the system, object and user meta fields,
respectively.

## Usage

``` r
field_system(x, field = NULL)

field_system(x, field = NULL) <- value

field_object(x, field = NULL)

field_object(x, field = NULL) <- value

field_user(x, field = NULL)

field_user(x, field = NULL) <- value
```

## Arguments

- x:

  a list of attributes extracted from a `dfm`, `tokens`, or `corpus`
  object by `attributes(x)`

- field:

  name of the sub-field to access or assign values
