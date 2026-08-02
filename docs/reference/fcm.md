# Create a feature co-occurrence matrix

Create a sparse feature co-occurrence matrix, measuring co-occurrences
of features within a user-defined context. The context can be defined as
a document or a window within a collection of documents, with an
optional vector of weights applied to the co-occurrence counts.

## Usage

``` r
fcm(
  x,
  context = c("document", "window"),
  count = c("frequency", "boolean", "weighted"),
  window = 5L,
  weights = NULL,
  ordered = FALSE,
  tri = TRUE,
  ...
)
```

## Arguments

- x:

  a [tokens](https://quanteda.io/reference/tokens.md), or
  [dfm](https://quanteda.io/reference/dfm.md) object from which to
  generate the feature co-occurrence matrix

- context:

  the context in which to consider term co-occurrence: `"document"` for
  co-occurrence counts within document; `"window"` for co-occurrence
  within a defined window of words, which requires a positive integer
  value for `window`. Note: if `x` is a dfm object, then `context` can
  only be `"document"`.

- count:

  how to count co-occurrences:

  `"frequency"`

  :   count the number of co-occurrences within the context

  `"boolean"`

  :   count only the co-occurrence or not within the context,
      irrespective of how many times it occurs.

  `"weighted"`

  :   count a weighted function of counts, typically as a function of
      distance from the target feature. Only makes sense for
      `context = "window"`.

- window:

  positive integer value for the size of a window on either side of the
  target feature, default is 5, meaning 5 words before and after the
  target feature

- weights:

  a vector of weights applied to each distance from `1:window`, strictly
  decreasing by default; can be a custom-defined vector of the same
  length as `window`

- ordered:

  if `TRUE`, count only the forward co-occurrences for each target token
  for bigram models, so that the `i, j` cell of the fcm is the number of
  times that token `j` occurs before the target token `i` within the
  window. Only makes sense for `context = "window"`, and when
  `ordered = TRUE`, the argument `tri` has no effect.

- tri:

  if `TRUE` return only upper triangle (including diagonal). Ignored if
  `ordered = TRUE`.

- ...:

  not used here

## Details

The function `fcm()` provides a very general implementation of a
"context-feature" matrix, consisting of a count of feature co-occurrence
within a defined context. This context, following Momtazi et. al.
(2010), can be defined as the *document*, *sentences* within documents,
*syntactic relationships* between features (nouns within a sentence, for
instance), or according to a *window*. When the context is a window, a
weighting function is typically applied that is a function of distance
from the target word (see Jurafsky and Martin 2015, Ch. 16) and ordered
co-occurrence of the two features is considered (see Church & Hanks
1990).

fcm provides all of this functionality, returning a \\V \* V\\ matrix
(where \\V\\ is the vocabulary size, returned by
[`nfeat()`](https://quanteda.io/reference/ndoc.md)). The `tri = TRUE`
option will only return the upper part of the matrix.

Unlike some implementations of co-occurrences, fcm counts feature
co-occurrences with themselves, meaning that the diagonal will not be
zero.

fcm also provides "boolean" counting within the context of "window",
which differs from the counting within "document".

`is.fcm(x)` returns `TRUE` if and only if its x is an object of type
fcm.

## References

Momtazi, S., Khudanpur, S., & Klakow, D. (2010). "A comparative study of
word co-occurrence for term clustering in language model-based sentence
retrieval. *Human Language Technologies: The 2010 Annual Conference of
the North American Chapter of the ACL*, Los Angeles, California, June
2010, 325-328. https://aclanthology.org/N10-1046/

Jurafsky, D. & Martin, J.H. (2018). From *Speech and Language
Processing: An Introduction to Natural Language Processing,
Computational Linguistics, and Speech Recognition*. Draft of September
23, 2018 (Chapter 6, Vector Semantics). Available at
<https://web.stanford.edu/~jurafsky/slp3/>.

Church, K. W. & P. Hanks (1990). Word association norms, mutual
information, and lexicography. *Computational Linguistics*, 16(1),
22-29. https://aclanthology.org/J90-1003/

## Author

Kenneth Benoit (R), Haiyan Wang (R, C++), Kohei Watanabe (C++)

## Examples

``` r
# see http://bit.ly/29b2zOA
toks1 <- tokens(c("A D A C E A D F E B A C E D"))
fcm(toks1, context = "window", window = 2)
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features A D C E F B
#>        A 2 3 3 4 1 1
#>        D 0 0 2 3 1 0
#>        C 0 0 0 2 0 1
#>        E 0 0 0 0 1 1
#>        F 0 0 0 0 0 1
#>        B 0 0 0 0 0 0
fcm(toks1, context = "window", count = "weighted", window = 3)
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features        A        D        C        E         F         B
#>        A 1.666667 3.333333 2.833333 2.833333 0.8333333 1.0000000
#>        D 0        0        1.333333 2.333333 1.0000000 0.3333333
#>        C 0        0        0        2.333333 0         0.5000000
#>        E 0        0        0        0        1.3333333 1.3333333
#>        F 0        0        0        0        0         0.5000000
#>        B 0        0        0        0        0         0        
fcm(toks1, context = "window", count = "weighted", window = 3,
    weights = c(3, 2, 1), ordered = TRUE, tri = FALSE)
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features A D C E F B
#>        A 3 7 7 5 2 0
#>        D 3 0 2 3 3 1
#>        C 2 3 0 6 0 0
#>        E 5 5 1 0 1 3
#>        F 1 0 0 3 0 2
#>        B 3 0 2 1 0 0

# with multiple documents
toks2 <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
fcm(toks2, context = "document", count = "frequency")
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features a b c e f g
#>        a 4 6 6 3 1 1
#>        b 0 1 2 0 0 0
#>        c 0 0 0 2 1 1
#>        e 0 0 0 0 1 1
#>        f 0 0 0 0 0 1
#>        g 0 0 0 0 0 0
fcm(toks2, context = "document", count = "boolean")
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features a b c e f g
#>        a 2 1 3 2 1 1
#>        b 0 1 1 0 0 0
#>        c 0 0 0 2 1 1
#>        e 0 0 0 0 1 1
#>        f 0 0 0 0 0 1
#>        g 0 0 0 0 0 0
fcm(toks2, context = "window", window = 2)
#> Feature co-occurrence matrix of: 6 by 6 features.
#>         features
#> features a b c e f g
#>        a 8 3 3 2 0 0
#>        b 0 2 2 0 0 0
#>        c 0 0 0 2 1 0
#>        e 0 0 0 0 1 1
#>        f 0 0 0 0 0 1
#>        g 0 0 0 0 0 0

txt3 <- c("The quick brown fox jumped over the lazy dog.",
         "The dog jumped and ate the fox.")
toks3 <- tokens(char_tolower(txt3), remove_punct = TRUE)
fcm(toks3, context = "document")
#> Feature co-occurrence matrix of: 10 by 10 features.
#>         features
#> features the quick brown fox jumped over lazy dog and ate
#>   the      2     2     2   4      4    2    2   4   2   2
#>   quick    0     0     1   1      1    1    1   1   0   0
#>   brown    0     0     0   1      1    1    1   1   0   0
#>   fox      0     0     0   0      2    1    1   2   1   1
#>   jumped   0     0     0   0      0    1    1   2   1   1
#>   over     0     0     0   0      0    0    1   1   0   0
#>   lazy     0     0     0   0      0    0    0   1   0   0
#>   dog      0     0     0   0      0    0    0   0   1   1
#>   and      0     0     0   0      0    0    0   0   0   1
#>   ate      0     0     0   0      0    0    0   0   0   0
fcm(toks3, context = "window", window = 3)
#> Feature co-occurrence matrix of: 10 by 10 features.
#>         features
#> features the quick brown fox jumped over lazy dog and ate
#>   the      0     1     1   3      3    1    1   2   2   1
#>   quick    0     0     1   1      1    0    0   0   0   0
#>   brown    0     0     0   1      1    1    0   0   0   0
#>   fox      0     0     0   0      1    1    0   0   1   1
#>   jumped   0     0     0   0      0    1    1   1   1   1
#>   over     0     0     0   0      0    0    1   1   0   0
#>   lazy     0     0     0   0      0    0    0   1   0   0
#>   dog      0     0     0   0      0    0    0   0   1   1
#>   and      0     0     0   0      0    0    0   0   0   1
#>   ate      0     0     0   0      0    0    0   0   0   0
```
