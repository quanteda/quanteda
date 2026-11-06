# Coercion and checking functions for dictionary objects

Convert a dictionary from a different format into a quanteda dictionary,
or check to see if an object is a dictionary.

## Usage

``` r
as.dictionary(x, ...)

# S3 method for class 'data.frame'
as.dictionary(x, format = c("tidytext"), separator = " ", tolower = FALSE, ...)

is.dictionary(x)
```

## Arguments

- x:

  a object to be coerced to a
  [dictionary](https://quanteda.io/reference/dictionary.md) object.

- ...:

  additional arguments passed to underlying functions.

- format:

  input format for the object to be coerced to a
  [dictionary](https://quanteda.io/reference/dictionary.md); current
  legal values are a data.frame with the fields `word` and `sentiment`
  (as per the **tidytext** package)

- separator:

  the character in between multi-word dictionary values. This defaults
  to `" "`.

- tolower:

  if `TRUE`, convert all dictionary values to lowercase.

## Value

`as.dictionary` returns a quanteda
[dictionary](https://quanteda.io/reference/dictionary.md) object. This
conversion function differs from the
[`dictionary()`](https://quanteda.io/reference/dictionary.md)
constructor function in that it converts an existing object rather than
creates one from components or from a file.

`is.dictionary` returns `TRUE` if an object is a quanteda
[dictionary](https://quanteda.io/reference/dictionary.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data(sentiments, package = "tidytext")
as.dictionary(subset(sentiments, lexicon == "nrc"))
as.dictionary(subset(sentiments, lexicon == "bing"))
# to convert AFINN into polarities - adjust thresholds if desired
datafinn <- subset(sentiments, lexicon == "AFINN")
datafinn[["sentiment"]] <-
    with(datafinn,
         sentiment <- ifelse(score < 0, "negative",
                             ifelse(score > 0, "positive", "netural"))
    )
with(datafinn, table(score, sentiment))
as.dictionary(datafinn)

dat <- data.frame(
    word = c("Great", "Horrible"),
    sentiment = c("positive", "negative")
    )
as.dictionary(dat)
as.dictionary(dat, tolower = FALSE)
} # }

is.dictionary(dictionary(list(key1 = c("val1", "val2"), key2 = "val3")))
#> [1] TRUE
# [1] TRUE
is.dictionary(list(key1 = c("val1", "val2"), key2 = "val3"))
#> [1] FALSE
# [1] FALSE
```
