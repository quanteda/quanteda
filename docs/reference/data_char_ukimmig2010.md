# Immigration-related sections of 2010 UK party manifestos

Extracts from the election manifestos of 9 UK political parties from
2010, related to immigration or asylum-seekers.

## Usage

``` r
data(data_char_ukimmig2010)
```

## Format

A named character vector of plain ASCII texts

## Examples

``` r
data_corpus_ukimmig2010 <- 
    corpus(data_char_ukimmig2010, 
           docvars = data.frame(party = names(data_char_ukimmig2010)))
summary(data_corpus_ukimmig2010, showmeta = TRUE)
#> Corpus consisting of 9 documents, showing 9 documents:
#> 
#>          Text Types Tokens Sentences        party
#>           BNP  1125   3280       136          BNP
#>     Coalition   142    260        12    Coalition
#>  Conservative   251    499        21 Conservative
#>        Greens   322    679        30       Greens
#>        Labour   298    683        33       Labour
#>        LibDem   251    483        26       LibDem
#>            PC    77    114         5           PC
#>           SNP    88    134         4          SNP
#>          UKIP   346    723        37         UKIP
#> 
```
