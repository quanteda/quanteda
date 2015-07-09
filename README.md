<!-- README.md is generated from README.Rmd. Please edit that file -->
quanteda: Quantitative Analysis of Textual Data
===============================================

An R package for managing and analyzing text, by Ken Benoit and Paul Nulty.

quanteda makes it easy to manage texts in the form of a corpus, defined as a collection of texts that includes document-level variables specific to each text, as well as meta-data for documents and for the collection as a whole. quanteda includes tools to make it easy and fast to manuipulate the texts in a corpus, for instance by tokenizing them, with or without stopwords or stemming, or to segment them by sentence or paragraph units.

quanteda implements bootstrapping methods for texts that makes it easy to resample texts from pre-defined units, to facilitate computation of confidence intervals on textual statistics using techniques of non-parametric bootstrapping, but applied to the original texts as data. quanteda includes a suite of sophisticated tools to extract features of the texts into a quantitative matrix, where these features can be defined according to a dictionary or thesaurus, including the declaration of collocations to be treated as single features.

Once converted into a quantitative matrix (known as a "dfm" for document-feature matrix), the textual feature can be analyzed using quantitative methods for describing, comparing, or scaling texts, or used to train machine learning methods for class prediction.

How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

1.  If you want the current CRAN release, you can install using your R package manager. If you are going to the trouble to install the package from GitHub, then you should be using the `dev` branch which contains the latest changes, feature additions, etc. To install the deveopment branch, you will need the `devtools` package. Once you've installed that (from CRAN), you can install `quanteda` using:

        devtools::install_github("kbenoit/quanteda", ref="dev")

2.  If that fails and you are using a Windows platform: To build the C++ parts of quanteda on Windows platforms, you will need also to install the [Rtools](http://cran.r-project.org/bin/windows/Rtools/) software available from CRAN. (OS X and Linux users can skip this step.)

3.  (Optional) You can install the additional corpus data from **quantedaData** using

    ``` s
    ## devtools required to install quanteda from Github
    devtools::install_github("kbenoit/quantedaData")
    ```

Documentation
-------------

In-depth tutorials in the form of a gitbook will be available here [here](http://kbenoit.github.io/quanteda).

Examples for any function can also be seen using (for instance, for `corpus()`):

``` s
example(corpus)
```

There are also some demo functions that show off some of the package capabilities, such as `demo(quanteda)`.

Example
-------

``` r
library(quanteda)
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(ukimmigTexts,
                            docvars=data.frame(party=names(ukimmigTexts)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
uk2010immigCorpus
#> Corpus consisting of 9 documents.
summary(uk2010immigCorpus, showmeta=TRUE)
#> Corpus consisting of 9 documents.
#> 
#>   Text Types Tokens Sentences        party
#>  text1  1024   2876       137          BNP
#>  text2   135    235        12    Coalition
#>  text3   235    454        21 Conservative
#>  text4   306    614        30       Greens
#>  text5   276    630        34       Labour
#>  text6   246    443        26       LibDem
#>  text7    74    103         5           PC
#>  text8    82    125         4          SNP
#>  text9   310    641        38         UKIP
#> 
#> Source:  /Users/kbenoit/Dropbox/QUANTESS/quanteda_kenlocal_gh/* on x86_64 by kbenoit.
#> Created: Thu Jul  9 17:59:51 2015.
#> Notes:   Immigration-related sections of 2010 UK party manifestos.

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
#>                                               preword         word
#>   [text1, 71]                further immigration, the deportation 
#>  [text1, 139]                            The BNP will    deport   
#> [text1, 1628] long-term resettlement programme.\n\n2.    Deport   
#> [text1, 1633]          illegal immigrants\n\nWe shall    deport   
#> [text1, 1653]                current unacceptably lax deportation 
#> [text1, 1659]                           of people are   deported  
#> [text1, 2169]                     enforced by instant deportation,
#> [text1, 2180]         British immigration laws.\n\n8. Deportation 
#> [text1, 2186]           Foreign Criminals\n\nWe shall    deport   
#> [text1, 2198]                       This includes the deportation 
#>  [text4, 566]                      subject to summary deportation.
#>  [text6, 194]         illegal labour.\n\n- Prioritise deportation 
#>  [text6, 394]                  flight risks.\n\n- End deportations
#>  [text9, 317]                            laws or face deportation.
#>                                   postword
#>   [text1, 71] of all illegal              
#>  [text1, 139] all foreigners convicted    
#> [text1, 1628] all illegal immigrants\n\nWe
#> [text1, 1633] all illegal immigrants      
#> [text1, 1653] policies, thousands of      
#> [text1, 1659] from the UK                 
#> [text1, 2169] for anyone found            
#> [text1, 2180] of all Foreign              
#> [text1, 2186] all criminal entrants,      
#> [text1, 2198] of all Muslim               
#>  [text4, 566] They should receive         
#>  [text6, 194] efforts on criminals,       
#>  [text6, 394] of refugees to              
#>  [text9, 317] Such citizens will

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, ignoredFeatures=c("will", stopwords("english")))
#> Creating a dfm from a corpus ...
#>    ... lowercasing
#>    ... tokenizing
#>    ... indexing 9 documents
#>    ... shaping tokens into data.table, found 6,021 total tokens
#>    ... ignoring 175 feature types, discarding 2,599 total features (43.2%)
#>    ... summing tokens by document
#>    ... indexing 1,477 feature types
#>    ... building sparse matrix
#>    ... created a 9 x 1477 sparse dfm
#>    ... complete. Elapsed time: 0.038 seconds.
dim(mydfm)              # basic dimensions of the dfm
#> [1]    9 1477
topfeatures(mydfm, 20)  # 20 top words
#> immigration     british      people      asylum     britain      system 
#>          68          38          36          29          28          27 
#>          uk  population     country         new      ensure  immigrants 
#>          27          21          20          19          17          17 
#>       shall citizenship        work          eu    national      social 
#>          17          16          15          14          14          14 
#>         bnp     illegal 
#>          13          13
if (Sys.info()['sysname']=="Darwin") quartz() # open nicer window, Mac only
plot(mydfm)             # word cloud     
```

![](README-quanteda_example-1.png)
