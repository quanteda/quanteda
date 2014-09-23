quanteda: Quantitative Analysis of Textual Data
===============================================

An R package for managing and analyzing text, by Ken Benoit and Paul Nulty.

quanteda makes it easy to manage texts in the form of a
corpus, defined as a collection of texts that includes document-level
variables specific to each text, as well as meta-data for documents
and for the collection as a whole. quanteda includes tools to make it
easy and fast to manuipulate the texts the texts in a corpus, for
instance by tokenizing them, with or without stopwords or stemming, or
to segment them by sentence or paragraph units. 

quanteda implements
bootstrapping methods for texts that makes it easy to resample texts
from pre-defined units, to facilitate computation of confidence
intervals on textual statistics using techniques of non-parametric
bootstrapping, but applied to the original texts as data. quanteda
includes a suite of sophisticated tools to extract features of the
texts into a quantitative matrix, where these features can be defined
according to a dictionary or thesaurus, including the declaration of
collocations to be treated as single features. 

Once converted into a
quantitative matrix (known as a "dfm" for document-feature matrix),
the textual feature can be analyzed using quantitative methods for
describing, comparing, or scaling texts, or used to train machine
learning methods for class prediction.


How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

This is done as follows:

```S
# devtools required to install quanteda from Github
if (!require(devtools)) install.packages("devtools")
library(devtools)
# install the latest version quanteda from Github
install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE)

# to install the latest dev branch version quanteda from Github use:
install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE, ref="dev")
```

Documentation
-------------

We are working on a package vignette, but for the moment the best documentation is 
found in the quanteda.pdf document, which is a formatted version of the man files
available for all of the quanteda functions.

Examples for any function can also be seen using (for instance, for `corpus()`):
```S
example(corpus)
```

There are also some demo functions that show off some of the package capabilities, such 
as `demo(quanteda)`.


Example
-------

```{r quanteda_example}
library(quanteda)
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(uk2010immig,
                            docvars=data.frame(party=names(uk2010immig)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
uk2010immigCorpus
summary(uk2010immigCorpus, showmeta=TRUE)

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, stopwords=TRUE)
dim(mydfm)              # basic dimensions of the dfm
topfeatures(mydfm, 20)  # 20 top words
plot(mydfm)             # word cloud     
```


