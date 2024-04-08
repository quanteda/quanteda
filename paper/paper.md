---
title: 'quanteda: An R package for the quantitative analysis of textual data'
authors:
- affiliation: 1
  name: Kenneth Benoit
  orcid: 0000-0002-0797-564X
- affiliation: 1
  name: Kohei Watanabe
  orcid: 0000-0001-6519-5265
- affiliation: 2
  name: Haiyan Wang
  orcid: 0000-0003-4992-4311
- affiliation: 3
  name: Paul Nulty
  orcid: 0000-0002-7214-4666
- affiliation: 4
  name: Adam Obeng
  orcid: 0000-0002-2906-4775
- affiliation: 5
  name: Stefan Müller
  orcid: 0000-0002-6315-4125
- affiliation: 1
  name: Akitaka Matsuo
  orcid: 0000-0002-3323-6330
date: "4 October 2018"
output:
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
- text mining
- natural language processing
affiliations:
- index: 1
  name: Department of Methodology, London School of Economics and Political Science
- index: 2
  name: De Beers Inc.
- index: 3
  name: Centre for Research in Arts, Social Science and Humanities, University of
    Cambridge
- index: 4
  name: Facebook, Inc. (work conducted at the Department of Methodology, London School
    of Economics and Political Science)
- index: 5
  name: Department of Political Science, Trinity College Dublin
---

# Summary

**quanteda** is an R package providing a comprehensive workflow and toolkit for
natural language processing tasks such as corpus management, tokenization,
analysis, and visualization. It has extensive functions for applying dictionary
analysis, exploring texts using keywords-in-context, computing document and
feature similarities, and discovering multi-word expressions through collocation
scoring. Based entirely on sparse operations, it provides highly efficient
methods for compiling document-feature matrices and for manipulating these or
using them in further quantitative analysis. Using C++ and multi-threading
extensively, **quanteda** is also considerably faster and more efficient than
other R and Python packages in processing large textual data.

The package is designed for R users needing to apply natural language
processing to texts, from documents to final analysis.  Its
capabilities match or exceed those provided in many end-user software
applications, many of which are expensive and not open source.  The package is
therefore of great benefit to researchers, students, and other analysts with
fewer financial resources.  While using **quanteda** requires R programming
knowledge, its API is designed to enable powerful, efficient analysis with a
minimum of steps.  By emphasizing consistent design, furthermore, **quanteda**
lowers the barriers to learning and using NLP and quantitative text analysis
even for proficient R programmers.

# Corpus management

**quanteda** makes it easy to manage texts in the form of a "corpus", which is
defined as a collection of texts that includes document-level variables specific
to each text, as well as meta-data for documents and for the collection as a
whole. With the package, users can easily segment texts by words, paragraphs,
sentences, or even user-supplied delimiters and tags, group them into larger
documents by document-level variables, or subset them based on logical
conditions or combinations of document-level variables.

# Natural language processing

**quanteda** is principally designed to allow users a fast and convenient method
to construct a document-feature matrix from a corpus with an ability to perform
the most common natural language processing tasks such as tokenizing, stemming,
forming n-grams, and selecting and weighting features.  With these functions, users
can easily remove stop words and stem words in numerous languages, select words
in a dictionary, and convert frequency counts into weights, for instance using 
_tf-idf_ scoring.

Using the ICU library in the **stringi** package [@stringi] for text processing,
**quanteda** correctly handles Unicode character sets for regular expression
matching and detecting word boundaries for tokenization. Once texts are tokenized,
**quanteda** maps tokens to a hash table of integers to increase processing speed while
reducing memory usage. Many of the text processing functions are parallelized
using the Intel TBB library.

# Models and textual statistics

**quanteda** is especially suited to research because it was designed from the
outset for the social scientific analysis of textual data. Its `textmodel_*`
functions provide native, highly efficient implementations of several text
analytic scaling methods, such as Wordscores [@lbg:2003], Wordfish
[@SlapinProksch2008], class affinity scaling [@PerryBenoit2017], and
correspondence analysis [@greenacre1984]. More general textmodel functions
include efficient implementations of a multinomial Naive Bayes classifier
designed specifically for textual data [@Manningetal2008] and latent semantic
analysis [@deerwester1990]. **quanteda** also works flexibly and efficiently
with dictionaries, and is distributed with the 2015 version of the Lexicoder
Sentiment Dictionary [@youngsoroka2012].

In addition to models, the package provides a variety of text statistics, 
such as frequency analysis, "keyness", lexical diversity, readability, and
similarity and distance of documents or features. These make use of the
sparsity of document-feature matrices -- often over 90% sparse -- and parallelism
for efficient, fast computation. **quanteda**  also provides methods for statistically
scoring collocations, useful in identifying multi-word expressions.

# Text visualization

The package provides extensive methods for visualizing textual analyses, via its
family of `textplot_*` functions. These are typically designed to take another
package object as an input, to produce a specific form of plot. For instance,
from a feature co-occurrence matrix, or `fcm`, we can directly plot a network
using `textplot_network()`:

```r
library("quanteda")

# construct the feature co-occurrence matrix
examplefcm <-
    tokens(data_corpus_irishbudget2010, remove_punct = TRUE) |>
    tokens_tolower() |>
    tokens_remove(stopwords("english"), padding = FALSE) |>
    fcm(context = "window", window = 5, tri = FALSE)

# choose 30 most frequency features
topfeats <- names(topfeatures(examplefcm, 30))

# select the top 30 features only, plot the network
set.seed(100)
textplot_network(fcm_select(examplefcm, topfeats), min_freq = 0.8)
```

![Feature co-occurrence network plot example.](networkplot.png)



# Package design

**quanteda** has been carefully designed with several key aims in mind.

_Consistency_.  **quanteda** functions and objects are named systematically such
that `corpus()`, `tokens()` and `dfm()` construct those object types, and that
`corpus_*()`, `tokens_*()` and `dfm_*()` functions return a modified version of these
objects. Naming consistency applies also to the extensive built-in data objects in the
package, whose names always start with `data_*` followed by object types. This
not only gives the users a clear overview of the package, but also makes the
package more reliable for other packages that depend on it.

_Accessibility_.  **quanteda** contains extensive manual pages structured around
the naming rules. Furthermore, there are references, package vignettes, examples,
and tutorials on the website at https://quanteda.io. 
These materials help beginner users understand how to use these functions for
basic operations and expert users how to combine the functions for advanced text
processing and analysis.

_Performance_.  **quanteda**'s performance is enhanced by token hashing and
parallel computation implemented in C++, permitting large and fast text analysis
even on computers with relatively limited resources (such as laptop computers).
Built to use sparse data structures, **quanteda** can efficiently performs
complex textual data analyses, such as computing distances, calculating feature
discrimination statistics (keyness), or model fitting, even for large
document-feature matrices.

_Transparency and reproducibility_.  **quanteda** is designed to facilitate rigorous,
transparent, and reproducible scientific analysis of text. Being open-source
software, its source code can be scrutinized and corrected by other experts. Its
functions are designed to encourage a reproducible workflow by linking successive 
processing tasks in a clear, readable manner.

_Compatibility with other packages_.  For analysis not provided by built-in
functions, users can move **quanteda** objects seamlessly to other packages,
such as the
**stm** package for structural topic models [@STM] or word embedding packages
like **text2vec** [@text2vec].  **quanteda** also works well with companion
packages such as **spacyr** [@spacyr], an R wrapper to the spaCy Python library
[@spacy2], and **readtext** [@readtext], a package for converting and importing
text files into R.

# Funding and Support

Created at the London School of Economics in 2013 with funding from the European
Research Council  (ERC-2011-StG 283794-QUANTESS), **quanteda** is now
supported by the [Quanteda Initiative](https://quanteda.org), a non-profit organization founded in 2018
to provide ongoing support for the "quanteda ecosystem" of open-source text
analysis software.

# References
