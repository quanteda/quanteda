# Trim a dfm using frequency threshold-based feature selection

Returns a document by feature matrix reduced in size based on document
and term frequency, usually in terms of a minimum frequency, but may
also be in terms of maximum frequencies. Setting a combination of
minimum and maximum frequencies will select features based on a range.

Feature selection is implemented by considering features across all
documents, by summing them for term frequency, or counting the documents
in which they occur for document frequency. Rank and quantile versions
of these are also implemented, for taking the first \\n\\ features in
terms of descending order of overall global counts or document
frequencies, or as a quantile of all frequencies.

## Usage

``` r
dfm_trim(
  x,
  min_termfreq = NULL,
  max_termfreq = NULL,
  termfreq_type = c("count", "prop", "rank", "quantile"),
  min_docfreq = NULL,
  max_docfreq = NULL,
  docfreq_type = c("count", "prop", "rank", "quantile"),
  max_n = NULL,
  sparsity = NULL,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md) object

- min_termfreq, max_termfreq:

  minimum/maximum values of feature frequencies across all documents,
  below/above which features will be removed

- termfreq_type:

  how `min_termfreq` and `max_termfreq` are interpreted. `"count"` sums
  the frequencies; `"prop"` divides the term frequencies by the total
  sum; `"rank"` is matched against the inverted ranking of features in
  terms of overall frequency, so that 1, 2, ... are the highest and
  second highest frequency features, and so on; `"quantile"` sets the
  cutoffs according to the quantiles (see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html)) of term
  frequencies.

- min_docfreq, max_docfreq:

  minimum/maximum values of a feature's document frequency, below/above
  which features will be removed

- docfreq_type:

  specify how `min_docfreq` and `max_docfreq` are interpreted. `"count"`
  is the same as `[docfreq](x, scheme = "count")`; `"prop"` divides the
  document frequencies by the total sum; `"rank"` is matched against the
  inverted ranking of document frequency, so that 1, 2, ... are the
  features with the highest and second highest document frequencies, and
  so on; `"quantile"` sets the cutoffs according to the quantiles (see
  [`quantile()`](https://rdrr.io/r/stats/quantile.html)) of document
  frequencies.

- max_n:

  the maximum number of features. Features are selected based on their
  frequencies, but the earlier elements in
  [featnames](https://quanteda.io/reference/featnames.md) or
  [types](https://quanteda.io/reference/types.md) are chosen when the
  frequencies are equal.

- sparsity:

  equivalent to `1 - min_docfreq`, included for comparison with tm

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

A [dfm](https://quanteda.io/reference/dfm.md) reduced in features (with
the same number of documents)

## Note

Trimming a [dfm](https://quanteda.io/reference/dfm.md) object is an
operation based on the *values* in the document-feature matrix. To
select subsets of a dfm based on the features themselves (meaning the
feature labels from
[`featnames()`](https://quanteda.io/reference/featnames.md)) – such as
those matching a regular expression, or removing features matching a
stopword list, use
[`dfm_select()`](https://quanteda.io/reference/dfm_select.md).

## See also

[`dfm_select()`](https://quanteda.io/reference/dfm_select.md),
[`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md)

## Examples

``` r
dfmat <- dfm(tokens(data_corpus_inaugural))

# keep only words occurring >= 10 times and in >= 2 documents
dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 2)
#> Document-feature matrix of: 60 documents, 1,561 features (69.07% sparse) and 4
#> docvars.
#>                  features
#> docs              fellow-citizens  of the senate and house representatives :
#>   1789-Washington               1  71 116      1  48     2               2 1
#>   1793-Washington               0  11  13      0   2     0               0 1
#>   1797-Adams                    3 140 163      1 130     0               2 0
#>   1801-Jefferson                2 104 130      0  81     0               0 1
#>   1805-Jefferson                0 101 143      0  93     0               0 0
#>   1809-Madison                  1  69 104      0  43     0               0 0
#>                  features
#> docs              among to
#>   1789-Washington     1 48
#>   1793-Washington     0  5
#>   1797-Adams          4 72
#>   1801-Jefferson      1 61
#>   1805-Jefferson      7 83
#>   1809-Madison        0 61
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 1,551 more
#> features ]

# keep only words occurring >= 10 times and in at least 0.4 of the documents
dfm_trim(dfmat, min_termfreq = 10, min_docfreq = 0.4, docfreq_type = "prop")
#> Document-feature matrix of: 60 documents, 367 features (35.91% sparse) and 4
#> docvars.
#>                  features
#> docs               of the and : among to life no could have
#>   1789-Washington  71 116  48 1     1 48    1  8     3   12
#>   1793-Washington  11  13   2 1     0  5    0  0     0    1
#>   1797-Adams      140 163 130 0     4 72    2  6     1    7
#>   1801-Jefferson  104 130  81 1     1 61    1  1     0   10
#>   1805-Jefferson  101 143  93 0     7 83    2  7     2   24
#>   1809-Madison     69 104  43 0     0 61    1  2     1    8
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 357 more
#> features ]

# keep only words occurring <= 10 times and in <=2 documents
dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 2)
#> Document-feature matrix of: 60 documents, 5,761 features (97.89% sparse) and 4
#> docvars.
#>                  features
#> docs              notification 14th month fondest predilection flattering
#>   1789-Washington            1    1     1       1            1          1
#>   1793-Washington            0    0     0       0            0          0
#>   1797-Adams                 0    0     0       0            0          0
#>   1801-Jefferson             0    0     0       0            0          0
#>   1805-Jefferson             0    0     0       0            0          0
#>   1809-Madison               0    0     0       0            0          0
#>                  features
#> docs              immutable asylum interruptions gradual
#>   1789-Washington         2      1             1       1
#>   1793-Washington         0      0             0       0
#>   1797-Adams              0      0             0       0
#>   1801-Jefferson          0      0             0       0
#>   1805-Jefferson          0      0             0       0
#>   1809-Madison            0      0             0       0
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 5,751 more
#> features ]

# keep only words occurring <= 10 times and in at most 3/4 of the documents
dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 0.75, docfreq_type = "prop")
#> Document-feature matrix of: 60 documents, 8,175 features (96.22% sparse) and 4
#> docvars.
#>                  features
#> docs              vicissitudes incident filled anxieties notification
#>   1789-Washington            1        1      1         1            1
#>   1793-Washington            0        0      0         0            0
#>   1797-Adams                 0        0      0         0            0
#>   1801-Jefferson             0        0      0         0            0
#>   1805-Jefferson             0        0      0         0            0
#>   1809-Madison               0        0      1         0            0
#>                  features
#> docs              transmitted 14th month summoned veneration
#>   1789-Washington           1    1     1        1          1
#>   1793-Washington           0    0     0        0          0
#>   1797-Adams                0    0     0        0          2
#>   1801-Jefferson            0    0     0        0          0
#>   1805-Jefferson            0    0     0        0          0
#>   1809-Madison              0    0     0        0          0
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 8,165 more
#> features ]

# keep only words occurring 5 times in 1000, and in 2 of 5 of documents
dfm_trim(dfmat, min_docfreq = 0.4, min_termfreq = 0.005, termfreq_type = "prop")
#> Document-feature matrix of: 60 documents, 26 features (0.77% sparse) and 4
#> docvars.
#>                  features
#> docs               of the and to have with that which by   ,
#>   1789-Washington  71 116  48 48   12   17   18    36 20  70
#>   1793-Washington  11  13   2  5    1    0    1     1  2   5
#>   1797-Adams      140 163 130 72    7   16   22    20 30 201
#>   1801-Jefferson  104 130  81 61   10   20   24    25 16 128
#>   1805-Jefferson  101 143  93 83   24   28   37    23 22 142
#>   1809-Madison     69 104  43 61    8   10    9    14 11  47
#> [ reached max_ndoc ... 54 more documents, reached max_nfeat ... 16 more
#> features ]

if (FALSE) { # \dontrun{
# compare to removeSparseTerms from the tm package
(dfmattm <- convert(dfmat, "tm"))
tm::removeSparseTerms(dfmattm, 0.7)
dfm_trim(dfmat, min_docfreq = 0.3)
dfm_trim(dfmat, sparsity = 0.7)
} # }
```
