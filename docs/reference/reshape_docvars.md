# Internal function to subset or duplicate docvar rows

Internal function to subset or duplicate docvar rows

## Usage

``` r
reshape_docvars(x, i = NULL, unique = FALSE, drop_docid = TRUE)
```

## Arguments

- x:

  docvar data.frame

- i:

  numeric or logical vector for subsetting/duplicating rows

- unique:

  if `TRUE`, names must be all unique. If `FALSE`, documents with the
  same names are treated as segments from the same document and given
  serial number.

- drop_docid:

  if `TRUE`, drop unused names of documents.
