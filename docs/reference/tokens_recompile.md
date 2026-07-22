# recompile a serialized tokens object

This function recompiles a serialized tokens object when the vocabulary
has been changed in a way that makes some of its types identical, such
as lowercasing when a lowercased version of the type already exists in
the type table, or introduces gaps in the integer map of the types. It
also re-indexes the types attribute to account for types that may have
become duplicates, through a procedure such as stemming or lowercasing;
or the addition of new tokens through compounding.

## Usage

``` r
tokens_recompile(x, method = c("C++", "R"))
```

## Arguments

- x:

  the [tokens](https://quanteda.io/reference/tokens.md) object to be
  recompiled

- method:

  `"C++"` for C++ implementation or `"R"` for an older R-based method

## Examples

``` r
# lowercasing
toks1 <- tokens(c(one = "a b c d A B C D",
                 two = "A B C d"))
attr(toks1, "types") <- char_tolower(attr(toks1, "types"))
unclass(toks1)
#> $one
#> [1] 1 2 3 4 5 6 7 8
#> 
#> $two
#> [1] 5 6 7 4
#> 
#> attr(,"types")
#> [1] "a" "b" "c" "d" "a" "b" "c" "d"
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1      one    one      1
#> 2      two    two      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 
unclass(quanteda:::tokens_recompile(toks1))
#> $one
#> [1] 1 2 3 4 5 6 7 8
#> 
#> $two
#> [1] 5 6 7 4
#> 
#> attr(,"types")
#> [1] "a" "b" "c" "d" "a" "b" "c" "d"
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1      one    one      1
#> 2      two    two      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 

# stemming
toks2 <- tokens("Stemming stemmed many word stems.")
unclass(toks2)
#> $text1
#> [1] 1 2 3 4 5 6
#> 
#> attr(,"types")
#> [1] "Stemming" "stemmed"  "many"     "word"     "stems"    "."       
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 
unclass(quanteda:::tokens_recompile(tokens_wordstem(toks2)))
#> $text1
#> [1] 1 2 3 4 2 5
#> 
#> attr(,"types")
#> [1] "Stem" "stem" "mani" "word" "."   
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 

# compounding
toks3 <- tokens("One two three four.")
unclass(toks3)
#> $text1
#> [1] 1 2 3 4 5
#> 
#> attr(,"types")
#> [1] "One"   "two"   "three" "four"  "."    
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 
unclass(tokens_compound(toks3, "two three"))
#> $text1
#> [1] 1 2 3 4 5
#> 
#> attr(,"types")
#> [1] "One"   "two"   "three" "four"  "."    
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 

# lookup
dict <- dictionary(list(test = c("one", "three")))
unclass(tokens_lookup(toks3, dict))
#> $text1
#> [1] 1 1
#> 
#> attr(,"types")
#> [1] "test"
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "dictionary"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 

# empty pads
unclass(tokens_select(toks3, dict))
#> $text1
#> [1] 1 2
#> 
#> attr(,"types")
#> [1] "One"   "three"
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 
unclass(tokens_select(toks3, dict, padding = TRUE))
#> $text1
#> [1] 1 0 2 0 0
#> 
#> attr(,"types")
#> [1] "One"   "three"
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 1
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 

# ngrams
unclass(tokens_ngrams(toks3, n = 2:3))
#> $text1
#> [1] 1 2 3 4 5 6 7
#> 
#> attr(,"types")
#> [1] "One_two"        "two_three"      "three_four"     "four_."        
#> [5] "One_two_three"  "two_three_four" "three_four_."  
#> attr(,"padding")
#> [1] TRUE
#> attr(,"docvars")
#>   docname_ docid_ segid_
#> 1    text1  text1      1
#> attr(,"meta")
#> attr(,"meta")$system
#> attr(,"meta")$system$`package-version`
#> [1] '4.4.1'
#> 
#> attr(,"meta")$system$`r-version`
#> [1] '4.6.1'
#> 
#> attr(,"meta")$system$system
#>   sysname   machine      user 
#> "Windows"  "x86-64"   "watan" 
#> 
#> attr(,"meta")$system$directory
#> [1] "C:/Users/watan/Repo/quanteda/docs/reference"
#> 
#> attr(,"meta")$system$created
#> [1] "2026-07-22"
#> 
#> 
#> attr(,"meta")$object
#> attr(,"meta")$object$unit
#> [1] "documents"
#> 
#> attr(,"meta")$object$what
#> [1] "word"
#> 
#> attr(,"meta")$object$tokenizer
#> [1] "tokenize_word4"
#> 
#> attr(,"meta")$object$ngram
#> [1] 2 3
#> 
#> attr(,"meta")$object$skip
#> [1] 0
#> 
#> attr(,"meta")$object$concatenator
#> [1] "_"
#> 
#> attr(,"meta")$object$summary
#> attr(,"meta")$object$summary$hash
#> character(0)
#> 
#> attr(,"meta")$object$summary$data
#> NULL
#> 
#> 
#> 
#> attr(,"meta")$user
#> list()
#> 
```
