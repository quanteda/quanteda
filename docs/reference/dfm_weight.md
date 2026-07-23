# Weight the feature frequencies in a dfm

Weight the feature frequencies in a dfm

## Usage

``` r
dfm_weight(
  x,
  scheme = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"),
  weights = NULL,
  base = 10,
  k = 0.5,
  smoothing = 0.5,
  force = FALSE
)

dfm_smooth(x, smoothing = 1)
```

## Arguments

- x:

  document-feature matrix created by
  [dfm](https://quanteda.io/reference/dfm.md)

- scheme:

  a label of the weight type:

  `count`

  : \\tf\_{ij}\\, an integer feature count (default when a dfm is
    created)

  `prop`

  : the proportion of the feature counts of total feature counts (aka
    relative frequency), calculated as \\tf\_{ij} / \sum_j tf\_{ij}\\

  `propmax`

  : the proportion of the feature counts of the highest feature count in
    a document, \\tf\_{ij} / \textrm{max}\_j tf\_{ij}\\

  `logcount`

  : take the 1 + the logarithm of each count, for the given base, or 0
    if the count was zero: \\1 + \textrm{log}\_{base}(tf\_{ij})\\ if
    \\tf\_{ij} \> 0\\, or 0 otherwise.

  `boolean`

  : recode all non-zero counts as 1

  `augmented`

  : equivalent to \\k + (1 - k) \*\\ `dfm_weight(x, "propmax")`

  `logave`

  : (1 + the log of the counts) / (1 + log of the average count within
    document), or \$\$\frac{1 + \textrm{log}\_{base} tf\_{ij}}{1 +
    \textrm{log}\_{base}(\sum_j tf\_{ij} / N_i)}\$\$

  `logsmooth`

  : log of the counts + `smooth`, or \\tf\_{ij} + s\\

- weights:

  if `scheme` is unused, then `weights` can be a named numeric vector of
  weights to be applied to the dfm, where the names of the vector
  correspond to feature labels of the dfm, and the weights will be
  applied as multipliers to the existing feature counts for the
  corresponding named features. Any features not named will be assigned
  a weight of 1.0 (meaning they will be unchanged).

- base:

  base for the logarithm when `scheme` is `"logcount"` or `logave`

- k:

  the k for the augmentation when `scheme = "augmented"`

- smoothing:

  constant added to the dfm cells for smoothing, default is 1 for
  `dfm_smooth()` and 0.5 for `dfm_weight()`

- force:

  logical; if `TRUE`, apply weighting scheme even if the dfm has been
  weighted before. This can result in invalid weights, such as as
  weighting by `"prop"` after applying `"logcount"`, or after having
  grouped a dfm using
  [`dfm_group()`](https://quanteda.io/reference/dfm_group.md).

## Value

`dfm_weight` returns the dfm with weighted values. Note the because the
default weighting scheme is `"count"`, simply calling this function on
an unweighted dfm will return the same object. Many users will want the
normalized dfm consisting of the proportions of the feature counts
within each document, which requires setting `scheme = "prop"`.

`dfm_smooth` returns a dfm whose values have been smoothed by adding the
`smoothing` amount. Note that this effectively converts a matrix from
sparse to dense format, so may exceed memory requirements depending on
the size of your input matrix.

## References

Manning, C.D., Raghavan, P., & Schütze, H. (2008). *An Introduction to
Information Retrieval*. Cambridge: Cambridge University Press.
<https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>

## See also

[`docfreq()`](https://quanteda.io/reference/docfreq.md)

## Examples

``` r
dfmat1 <- dfm(tokens(data_corpus_inaugural))

dfmat2 <- dfm_weight(dfmat1, scheme = "prop")
topfeatures(dfmat2)
#>       the         ,        of       and         .        to        in       our 
#> 3.8757348 2.8919962 2.7450775 2.1618834 2.0959837 1.8189031 1.0969121 0.9128931 
#>         a        we 
#> 0.8939534 0.8335764 
dfmat3 <- dfm_weight(dfmat1)
topfeatures(dfmat3)
#>   the     ,    of   and     .    to    in     a   our    we 
#> 10309  7394  7271  5550  5334  4678  2856  2342  2295  1912 
dfmat4 <- dfm_weight(dfmat1, scheme = "logcount")
topfeatures(dfmat4)
#>      the        ,       of      and        .       to       in        a 
#> 188.2903 180.8299 179.2293 173.3189 171.5695 168.9676 155.4927 148.9649 
#>      our     that 
#> 146.2271 143.9651 
dfmat5 <- dfm_weight(dfmat1, scheme = "logave")
topfeatures(dfmat5)
#>       the         ,        of       and         .        to        in         a 
#> 126.00955 120.92488 119.92173 115.87171 114.65954 113.04011 103.91105  99.27864 
#>       our      that 
#>  97.41927  96.16014 

# combine these methods for more complex dfm_weightings, e.g. as in Section 6.4
# of Introduction to Information Retrieval
head(dfm_tfidf(dfmat1, scheme_tf = "logcount"))
#> Document-feature matrix of: 6 documents, 9,591 features (93.93% sparse) and 4
#> docvars.
#>                  features
#> docs              fellow-citizens of the    senate and    house representatives
#>   1789-Washington       0.4993976  0   0 0.8239087   0 1.138481       0.8222812
#>   1793-Washington       0          0   0 0           0 0              0        
#>   1797-Adams            0.7376709  0   0 0.8239087   0 0              0.8222812
#>   1801-Jefferson        0.6497313  0   0 0           0 0              0        
#>   1805-Jefferson        0          0   0 0           0 0              0        
#>   1809-Madison          0.4993976  0   0 0           0 0              0        
#>                  features
#> docs                      :     among vicissitudes
#>   1789-Washington 0.1983677 0.1446828     1.079181
#>   1793-Washington 0.1983677 0             0       
#>   1797-Adams      0         0.2317905     0       
#>   1801-Jefferson  0.1983677 0.1446828     0       
#>   1805-Jefferson  0         0.2669539     0       
#>   1809-Madison    0         0             0       
#> [ reached max_nfeat ... 9,581 more features ]

# smooth the dfm
dfmat <- dfm(tokens(data_corpus_inaugural))
dfm_smooth(dfmat, 0.5)
#> Document-feature matrix of: 60 documents, 9,591 features (0.00% sparse) and 4
#> docvars.
#>                  features
#> docs              fellow-citizens    of   the senate   and house
#>   1789-Washington             1.5  71.5 116.5    1.5  48.5   2.5
#>   1793-Washington             0.5  11.5  13.5    0.5   2.5   0.5
#>   1797-Adams                  3.5 140.5 163.5    1.5 130.5   0.5
#>   1801-Jefferson              2.5 104.5 130.5    0.5  81.5   0.5
#>   1805-Jefferson              0.5 101.5 143.5    0.5  93.5   0.5
#>   1809-Madison                1.5  69.5 104.5    0.5  43.5   0.5
#>                  features
#> docs              representatives   : among vicissitudes
#>   1789-Washington             2.5 1.5   1.5          1.5
#>   1793-Washington             0.5 1.5   0.5          0.5
#>   1797-Adams                  2.5 0.5   4.5          0.5
#>   1801-Jefferson              0.5 1.5   1.5          0.5
#>   1805-Jefferson              0.5 0.5   7.5          0.5
#>   1809-Madison                0.5 0.5   0.5          0.5
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 9,581 more
#> features ]
```
