# Lexicoder Sentiment Dictionary (2015)

The 2015 Lexicoder Sentiment Dictionary in quanteda
[dictionary](https://quanteda.io/reference/dictionary.md) format.

## Usage

``` r
data(data_dictionary_LSD2015)
```

## Format

A [dictionary](https://quanteda.io/reference/dictionary.md) of four keys
containing glob-style [pattern
matches](https://quanteda.io/reference/valuetype.md).

- `negative`:

  2,858 word patterns indicating negative sentiment

- `positive`:

  1,709 word patterns indicating positive sentiment

- `neg_positive`:

  1,721 word patterns indicating a positive word preceded by a negation
  (used to convey negative sentiment)

- `neg_negative`:

  2,860 word patterns indicating a negative word preceded by a negation
  (used to convey positive sentiment)

## Details

The dictionary consists of 2,858 "negative" sentiment words and 1,709
"positive" sentiment words. A further set of 2,860 and 1,721 negations
of negative and positive words, respectively, is also included. While
many users will find the non-negation sentiment forms of the LSD
adequate for sentiment analysis, Young and Soroka (2012) did find a
small, but non-negligible increase in performance when accounting for
negations. Users wishing to test this or include the negations are
encouraged to subtract negated positive words from the count of positive
words, and subtract the negated negative words from the negative count.

Young and Soroka (2012) also suggest the use of a pre-processing script
to remove specific cases of some words (i.e., "good bye", or "nobody
better", which should not be counted as positive). Pre-processing
scripts are available at <https://www.snsoroka.com/data-lexicoder/>.

## License and Conditions

The LSD is available for non-commercial academic purposes only. By using
`data_dictionary_LSD2015`, you accept these terms.

Please cite the references below when using the dictionary.

## References

The objectives, development and reliability of the dictionary are
discussed in detail in Young and Soroka (2012). Please cite this article
when using the Lexicoder Sentiment Dictionary and related resources.
Young, L. & Soroka, S. (2012). *Lexicoder Sentiment Dictionary*.
Available at <https://www.snsoroka.com/data-lexicoder/>.

Young, L. & Soroka, S. (2012). Affective News: The Automated Coding of
Sentiment in Political Texts.
[doi:10.1080/10584609.2012.671234](https://doi.org/10.1080/10584609.2012.671234)
. *Political Communication*, 29(2), 205–231.

## Examples

``` r
# simple example
txt <- "This aggressive policy will not win friends."

tokens_lookup(tokens(txt), dictionary = data_dictionary_LSD2015, exclusive = FALSE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "This"         "NEGATIVE"     "policy"       "will"         "NEG_POSITIVE"
#> [6] "POSITIVE"     "POSITIVE"     "."           
#> 
## tokens from 1 document.
## text1 :
## [1] "This"   "NEGATIVE"   "policy"   "will"   "NEG_POSITIVE"   "POSITIVE"   "POSITIVE" "."

# notice that double-counting of negated and non-negated terms is avoided 
# when using nested_scope = "dictionary"
tokens_lookup(tokens(txt), dictionary = data_dictionary_LSD2015, 
              exclusive = FALSE, nested_scope = "dictionary")
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "This"         "NEGATIVE"     "policy"       "will"         "NEG_POSITIVE"
#> [6] "POSITIVE"     "."           
#> 
## tokens from 1 document.
## text1 :
## [1] "This"   "NEGATIVE"   "policy"   "will"   "NEG_POSITIVE" "POSITIVE."   

# compound neg_negative and neg_positive tokens before creating a dfm object
toks <- tokens_compound(tokens(txt), data_dictionary_LSD2015)

dfm_lookup(dfm(toks), data_dictionary_LSD2015)
#> Document-feature matrix of: 1 document, 4 features (25.00% sparse) and 0
#> docvars.
#>        features
#> docs    negative positive neg_positive neg_negative
#>   text1        1        1            1            0
```
