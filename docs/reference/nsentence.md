# Count the number of sentences

**\[deprecated\]**

Return the count of sentences in a corpus or character object.

## Usage

``` r
nsentence(x)
```

## Arguments

- x:

  a character or [corpus](https://quanteda.io/reference/corpus.md) whose
  sentences will be counted

## Value

count(s) of the total sentences per text

## Note

`nsentence()` is now deprecated for all usages except tokens objects
that have already been tokenised with `tokens(x, what = "sentence")`.
Using it on character or corpus objects will now generate a warning.

`nsentence()` relies on the boundaries definitions in the stringi
package (see
[stri_opts_brkiter](https://rdrr.io/pkg/stringi/man/stri_opts_brkiter.html)).
It does not count sentences correctly if the text has been transformed
to lower case, and for this reason `nsentence()` will issue a warning if
it detects all lower-cased text.

## Examples

``` r
# simple example
txt <- c(text1 = "This is a sentence: second part of first sentence.",
         text2 = "A word. Repeated repeated.",
         text3 = "Mr. Jones has a PhD from the LSE.  Second sentence.")
tokens(txt, what = "sentence") |>
    nsentence()
#> text1 text2 text3 
#>     1     2     2 
```
