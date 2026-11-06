# Convert a dfm to a data.frame

**\[superseded\]**

Defunct function to convert a dfm into a data.frame. Use
`convert(x, to = "data.frame")` instead.

## Usage

``` r
# S3 method for class 'dfm'
as.data.frame(
  x,
  row.names = NULL,
  ...,
  document = docnames(x),
  docid_field = "doc_id",
  check.names = FALSE
)
```

## Arguments

- x:

  any R object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- ...:

  unused

- document:

  optional first column of mode `character` in the data.frame, defaults
  `docnames(x)`. Set to `NULL` to exclude.

- docid_field:

  character; the name of the column containing document names used when
  `to = "data.frame"`. Unused for other conversions.

- check.names:

  logical; passed to the
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) call.

## See also

[`convert()`](https://quanteda.io/reference/convert.md)
