Compare
-------

``` r
toks_old <- tokenize(toLower(data_char_inaugural[c(2, 40)]), removePunct = TRUE)
toks_new <- tokens(toLower(data_char_inaugural[c(2, 40)]), removePunct = TRUE)
```

Hashed version is definitely smaller:

``` r
object.size(toks_old)
```

    ## 24176 bytes

``` r
object.size(toks_new)
```

    ## 22200 bytes

So where is the hashing??
-------------------------

but the tokens don't appear to be hashed, although it does have the `types` attribute:

``` r
str(toks_old)
```

    ## List of 2
    ##  $ 1793-Washington: chr [1:135] "fellow" "citizens" "i" "am" ...
    ##  $ 1945-Roosevelt : chr [1:557] "chief" "justice" "mr" "vice" ...
    ##  - attr(*, "class")= chr [1:2] "tokenizedTexts" "list"
    ##  - attr(*, "what")= chr "word"
    ##  - attr(*, "ngrams")= int 1
    ##  - attr(*, "concatenator")= chr ""

``` r
str(toks_new)
```

    ## List of 2
    ##  $ 1793-Washington: chr [1:135] "fellow" "citizens" "i" "am" ...
    ##  $ 1945-Roosevelt : chr [1:557] "chief" "justice" "mr" "vice" ...
    ##  - attr(*, "class")= chr [1:3] "tokens" "tokenizedTexts" "list"
    ##  - attr(*, "types")= chr [1:310] "fellow" "citizens" "i" "am" ...
    ##  - attr(*, "what")= chr "word"
    ##  - attr(*, "ngrams")= int 1
    ##  - attr(*, "concatenator")= chr ""

``` r
is.character(toks_new[[1]])
```

    ## [1] TRUE

Or look at this:

``` r
str(quanteda::tokens_hash(tokenize(c("this is a sample sentence to tokenize"))))
```

    ## List of 1
    ##  $ : chr [1:7] "this" "is" "a" "sample" ...
    ##  - attr(*, "class")= chr [1:3] "tokens" "tokenizedTexts" "list"
    ##  - attr(*, "what")= chr "word"
    ##  - attr(*, "ngrams")= int 1
    ##  - attr(*, "concatenator")= chr ""
    ##  - attr(*, "types")= chr [1:7] "this" "is" "a" "sample" ...

But it all seems to work...
---------------------------

All code using the hashed tokens appears to work still:

``` r
txt <- c(doc1 = "The quick brown fox jumped over the lazy dog.",
         doc2 = "The dog jumped and ate the fox.")
toks_new <- tokens(toLower(txt), removePunct = TRUE)
tokens_select(toks_new, "the")
```

    ## tokens from 2 documents.
    ## doc1 :
    ## [1] "the" "the"
    ## 
    ## doc2 :
    ## [1] "the" "the"

``` r
tokens_compound(toks_new, list(c("^jump", ".+")), valuetype = "regex")
```

    ## tokens from 2 documents.
    ## doc1 :
    ## [1] "the"         "quick"       "brown"       "fox"         "jumped_over"
    ## [6] "the"         "lazy"        "dog"        
    ## 
    ## doc2 :
    ## [1] "the"        "dog"        "jumped_and" "ate"        "the"       
    ## [6] "fox"

``` r
tokens_ngrams(toks_new, n = 1:2, skip = 0:1)
```

    ## tokens from 2 documents.
    ## doc1 :
    ##  [1] "the"          "quick"        "brown"        "fox"         
    ##  [5] "jumped"       "over"         "the"          "lazy"        
    ##  [9] "dog"          "the_quick"    "the_brown"    "quick_brown" 
    ## [13] "quick_fox"    "brown_fox"    "brown_jumped" "fox_jumped"  
    ## [17] "fox_over"     "jumped_over"  "jumped_the"   "over_the"    
    ## [21] "over_lazy"    "the_lazy"     "the_dog"      "lazy_dog"    
    ## 
    ## doc2 :
    ##  [1] "the"        "dog"        "jumped"     "and"        "ate"       
    ##  [6] "the"        "fox"        "the_dog"    "the_jumped" "dog_jumped"
    ## [11] "dog_and"    "jumped_and" "jumped_ate" "and_ate"    "and_the"   
    ## [16] "ate_the"    "ate_fox"    "the_fox"

Can you solve the mystery?
--------------------------
