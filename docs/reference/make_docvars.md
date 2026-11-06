# Internal function to make new system-level docvars

Internal function to make new system-level docvars

## Usage

``` r
make_docvars(n, docname = NULL, unique = TRUE, drop_docid = TRUE)
```

## Arguments

- n:

  the number of documents

- docname:

  a character vector for the names of documents. Must be the same length
  as `n` or `NULL`. If NULL, names are generated automatically.

- unique:

  if `TRUE`, names must be all unique. If `FALSE`, documents with the
  same names are treated as segments from the same document and given
  serial number.

- drop_docid:

  if `TRUE`, drop unused names of documents.
