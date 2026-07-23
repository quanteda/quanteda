# Virtual class "dfm" for a document-feature matrix

The dfm class of object is a type of
[Matrix-class](https://rdrr.io/pkg/Matrix/man/Matrix-class.html) object
with additional slots, described below. quanteda uses two subclasses of
the `dfm` class, depending on whether the object can be represented by a
sparse matrix, in which case it is a `dfm` class object, or if dense,
then a `dfmDense` object. See Details.

## Usage

``` r
# S4 method for class 'dfm'
t(x)

# S4 method for class 'dfm'
colSums(x, na.rm = FALSE, dims = 1L, ...)

# S4 method for class 'dfm'
rowSums(x, na.rm = FALSE, dims = 1L, ...)

# S4 method for class 'dfm'
colMeans(x, na.rm = FALSE, dims = 1L, ...)

# S4 method for class 'dfm'
rowMeans(x, na.rm = FALSE, dims = 1L, ...)

# S4 method for class 'dfm,numeric'
Arith(e1, e2)

# S4 method for class 'numeric,dfm'
Arith(e1, e2)

# S4 method for class 'dfm'
drop0(x, tol = 0)

# S4 method for class 'dfm,index,index,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,index,index,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,missing,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,missing,missing,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,index,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,index,missing,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,missing,index,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dfm,missing,index,logical'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  the dfm object

- na.rm:

  if `TRUE`, omit missing values (including `NaN`) from the calculations

- dims:

  ignored

- ...:

  additional arguments not used here

- e1:

  first quantity in an
  [Arith](https://rdrr.io/r/methods/S4groupGeneric.html) operation for
  dfm

- e2:

  second quantity in an
  [Arith](https://rdrr.io/r/methods/S4groupGeneric.html) operation for
  dfm

- i:

  document names or indices for documents to extract.

- j:

  feature names or indices for documents to extract.

## Details

The `dfm` class is a virtual class that will contain
[dgCMatrix-class](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html).

## Slots

- `weightTf`:

  the type of term frequency weighting applied to the dfm. Default is
  `"frequency"`, indicating that the values in the cells of the dfm are
  simple feature counts. To change this, use the
  [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md) method.

- `weightFf`:

  the type of document frequency weighting applied to the dfm. See
  [`docfreq()`](https://quanteda.io/reference/docfreq.md).

- `smooth`:

  a smoothing parameter, defaults to zero. Can be changed using the
  [`dfm_smooth()`](https://quanteda.io/reference/dfm_weight.md) method.

- `Dimnames`:

  These are inherited from
  [Matrix-class](https://rdrr.io/pkg/Matrix/man/Matrix-class.html) but
  are named `docs` and `features` respectively.

## See also

[dfm](https://quanteda.io/reference/dfm.md)

## Examples

``` r
# dfm subsetting
dfmat <- dfm(tokens(c("this contains lots of stopwords",
                  "no if, and, or but about it: lots",
                  "and a third document is it"),
                remove_punct = TRUE))
dfmat[1:2, ]
#> Document-feature matrix of: 2 documents, 16 features (59.38% sparse) and 0
#> docvars.
#>        features
#> docs    this contains lots of stopwords no if and or but
#>   text1    1        1    1  1         1  0  0   0  0   0
#>   text2    0        0    1  0         0  1  1   1  1   1
#> [ reached max_nfeat ... 6 more features ]
dfmat[1:2, 1:5]
#> Document-feature matrix of: 2 documents, 5 features (40.00% sparse) and 0
#> docvars.
#>        features
#> docs    this contains lots of stopwords
#>   text1    1        1    1  1         1
#>   text2    0        0    1  0         0
```
