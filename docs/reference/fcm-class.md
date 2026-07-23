# Virtual class "fcm" for a feature co-occurrence matrix

The fcm class of object is a special type of
[fcm](https://quanteda.io/reference/fcm.md) object with additional
slots, described below.

## Usage

``` r
# S4 method for class 'fcm'
t(x)

# S4 method for class 'fcm,numeric'
Arith(e1, e2)

# S4 method for class 'numeric,fcm'
Arith(e1, e2)

# S4 method for class 'fcm'
drop0(x, tol = 0)

# S4 method for class 'fcm,index,index,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,index,index,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,missing,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,missing,missing,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,index,missing,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,index,missing,logical'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,missing,index,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'fcm,missing,index,logical'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  the fcm object

- e1:

  first quantity in "+" operation for fcm

- e2:

  second quantity in "+" operation for fcm

- i:

  index for features

- j:

  index for features

- ...:

  additional arguments not used here

- drop:

  always set to `FALSE`

## Slots

- `context`:

  the context definition

- `window`:

  the size of the window, if `context = "window"`

- `count`:

  how co-occurrences are counted

- `weights`:

  context weighting for distance from target feature, equal in length to
  `window`

- `margin`:

  frequencies of features in the original
  [dfm](https://quanteda.io/reference/dfm.md) or
  [tokens](https://quanteda.io/reference/tokens.md)

- `tri`:

  whether the lower triangle of the symmetric \\V \times V\\ matrix is
  recorded

- `ordered`:

  whether a term appears before or after the target feature are counted
  separately

## See also

[fcm](https://quanteda.io/reference/fcm.md)

## Examples

``` r
# fcm subsetting
fcmat <- fcm(tokens(c("this contains lots of stopwords",
                  "no if, and, or but about it: lots"),
                remove_punct = TRUE))
fcmat[1:3, ]
#> Feature co-occurrence matrix of: 3 by 12 features.
#>           features
#> features   this contains lots of stopwords no if and or but
#>   this        0        1    1  1         1  0  0   0  0   0
#>   contains    0        0    1  1         1  0  0   0  0   0
#>   lots        0        0    0  1         1  1  1   1  1   1
#> [ reached max_nfeat ... 2 more features ]
fcmat[4:5, 1:5]
#> Feature co-occurrence matrix of: 2 by 5 features.
#>            features
#> features    this contains lots of stopwords
#>   of           0        0    0  0         1
#>   stopwords    0        0    0  0         0

```
