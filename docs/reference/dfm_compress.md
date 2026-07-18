# Recombine a dfm or fcm by combining identical dimension elements

"Compresses" or groups a [dfm](https://quanteda.io/reference/dfm.md) or
[fcm](https://quanteda.io/reference/fcm.md) whose dimension names are
the same, for either documents or features. This may happen, for
instance, if features are made equivalent through application of a
thesaurus. It could also be needed after a
[`cbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md) or
[`rbind.dfm()`](https://quanteda.io/reference/cbind.dfm.md) operation.
In most cases, you will not need to call `dfm_compress`, since it is
called automatically by functions that change the dimensions of the dfm,
e.g. [`dfm_tolower()`](https://quanteda.io/reference/dfm_tolower.md).

## Usage

``` r
dfm_compress(
  x,
  margin = c("both", "documents", "features"),
  verbose = quanteda_options("verbose")
)

fcm_compress(x)
```

## Arguments

- x:

  input object, a [dfm](https://quanteda.io/reference/dfm.md) or
  [fcm](https://quanteda.io/reference/fcm.md)

- margin:

  character indicating on which margin to compress a dfm, either
  `"documents"`, `"features"`, or `"both"` (default). For fcm objects,
  `"documents"` has no effect.

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Value

`dfm_compress` returns a [dfm](https://quanteda.io/reference/dfm.md)
whose dimensions have been recombined by summing the cells across
identical dimension names
([docnames](https://quanteda.io/reference/docnames.md) or
[featnames](https://quanteda.io/reference/featnames.md)). The
[docvars](https://quanteda.io/reference/docvars.md) will be preserved
for combining by features but not when documents are combined.

`fcm_compress` returns an [fcm](https://quanteda.io/reference/fcm.md)
whose features have been recombined by combining counts of identical
features, summing their counts.

## Note

`fcm_compress` works only when the
[fcm](https://quanteda.io/reference/fcm.md) was created with a document
context.

## Examples

``` r
# dfm_compress examples
dfmat <- rbind(dfm(tokens(c("b A A", "C C a b B")), tolower = FALSE),
               dfm(tokens("A C C C C C"), tolower = FALSE))
colnames(dfmat) <- char_tolower(featnames(dfmat))
dfmat
#> Document-feature matrix of: 3 documents, 5 features (46.67% sparse) and 0
#> docvars.
#>          features
#> docs      b a c a b
#>   text1.1 1 2 0 0 0
#>   text2.1 1 0 2 1 1
#>   text1.2 0 1 5 0 0
dfm_compress(dfmat, margin = "documents")
#> Document-feature matrix of: 2 documents, 5 features (30.00% sparse) and 0
#> docvars.
#>        features
#> docs    b a c a b
#>   text1 1 3 5 0 0
#>   text2 1 0 2 1 1
dfm_compress(dfmat, margin = "features")
#> Document-feature matrix of: 3 documents, 3 features (22.22% sparse) and 0
#> docvars.
#>          features
#> docs      b a c
#>   text1.1 1 2 0
#>   text2.1 2 1 2
#>   text1.2 0 1 5
dfm_compress(dfmat)
#> Document-feature matrix of: 2 documents, 3 features (0.00% sparse) and 0
#> docvars.
#>        features
#> docs    b a c
#>   text1 1 3 5
#>   text2 2 1 2

# no effect if no compression needed
dfmatsubset <- dfm(tokens(data_corpus_inaugural[1:5]))
dim(dfmatsubset)
#> [1]    5 1948
dim(dfm_compress(dfmatsubset))
#> [1]    5 1948

# compress an fcm
fcmat1 <- fcm(tokens("A D a C E a d F e B A C E D"),
             context = "window", window = 3)
## this will produce an error:
# fcm_compress(fcmat1)

txt <- c("The fox JUMPED over the dog.",
         "The dog jumped over the fox.")
toks <- tokens(txt, remove_punct = TRUE)
fcmat2 <- fcm(toks, context = "document")
colnames(fcmat2) <- rownames(fcmat2) <- tolower(colnames(fcmat2))
colnames(fcmat2)[5] <- rownames(fcmat2)[5] <- "fox"
fcmat2
#> Feature co-occurrence matrix of: 7 by 7 features.
#>         features
#> features the fox jumped over fox dog jumped
#>   the      0   2      1    2   2   2      1
#>   fox      0   0      1    2   2   2      1
#>   jumped   0   0      0    1   1   1      0
#>   over     0   0      0    0   2   2      1
#>   fox      0   0      0    0   0   2      1
#>   dog      0   0      0    0   0   0      1
#>   jumped   0   0      0    0   0   0      0
fcm_compress(fcmat2)
#> Feature co-occurrence matrix of: 5 by 5 features.
#>         features
#> features the fox jumped over dog
#>   the      0   4      2    2   2
#>   fox      0   2      3    2   4
#>   jumped   0   1      0    1   1
#>   over     0   2      1    0   2
#>   dog      0   0      1    0   0
```
