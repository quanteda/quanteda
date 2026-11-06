# Get the feature labels from a dfm

Get the features from a document-feature matrix, which are stored as the
column names of the [dfm](https://quanteda.io/reference/dfm.md) object.

## Usage

``` r
featnames(x)
```

## Arguments

- x:

  the dfm whose features will be extracted

## Value

character vector of the feature labels

## Examples

``` r
dfmat <- dfm(tokens(data_corpus_inaugural))

# first 50 features (in original text order)
head(featnames(dfmat), 50)
#>  [1] "fellow-citizens" "of"              "the"             "senate"         
#>  [5] "and"             "house"           "representatives" ":"              
#>  [9] "among"           "vicissitudes"    "incident"        "to"             
#> [13] "life"            "no"              "event"           "could"          
#> [17] "have"            "filled"          "me"              "with"           
#> [21] "greater"         "anxieties"       "than"            "that"           
#> [25] "which"           "notification"    "was"             "transmitted"    
#> [29] "by"              "your"            "order"           ","              
#> [33] "received"        "on"              "14th"            "day"            
#> [37] "present"         "month"           "."               "one"            
#> [41] "hand"            "i"               "summoned"        "my"             
#> [45] "country"         "whose"           "voice"           "can"            
#> [49] "never"           "hear"           

# first 50 features alphabetically
head(sort(featnames(dfmat)), 50)
#>  [1] "!"           "\""          "$"           "'"           "("          
#>  [6] ")"           ","           "-"           "."           "/"          
#> [11] "1"           "1,000"       "100"         "100,000,000" "108"        
#> [16] "11"          "120,000,000" "125"         "13"          "14th"       
#> [21] "15th"        "16"          "1774"        "1776"        "1778"       
#> [26] "1780"        "1787"        "1789"        "1790"        "1798"       
#> [31] "1800"        "1801"        "1812"        "1815"        "1816"       
#> [36] "1817"        "1818"        "1826"        "1850"        "1861"       
#> [41] "1863"        "1868"        "1873"        "1880"        "1886"       
#> [46] "1890"        "1893"        "1896"        "1897"        "1898"       

# contrast with descending total frequency order from topfeatures()
names(topfeatures(dfmat, 50))
#>  [1] "the"        ","          "of"         "and"        "."         
#>  [6] "to"         "in"         "a"          "our"        "we"        
#> [11] "that"       "be"         "is"         "it"         "for"       
#> [16] "by"         "have"       "will"       "which"      "not"       
#> [21] "with"       "as"         "i"          "this"       "all"       
#> [26] "are"        "their"      "but"        "has"        "from"      
#> [31] "people"     "its"        "government" "or"         ";"         
#> [36] "on"         "my"         "us"         "been"       "can"       
#> [41] "no"         "they"       "so"         "an"         "-"         
#> [46] "who"        "must"       "upon"       "at"         "great"     
```
