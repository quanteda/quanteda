# An R package for the quantitative analysis of textual data

Functions for creating and managing textual corpora, extracting features
from textual data, and analyzing those features using quantitative
methods.

A fast, flexible, and comprehensive framework for quantitative text
analysis in R. Provides functionality for corpus management, creating
and manipulating tokens and n-grams, exploring keywords in context,
forming and manipulating sparse matrices of documents by features and
feature co-occurrences, analyzing keywords, computing feature
similarities and distances, applying content dictionaries, applying
supervised and unsupervised machine learning, visually representing text
and text analyses, and more.

## Details

quanteda makes it easy to manage texts in the form of a corpus, defined
as a collection of texts that includes document-level variables specific
to each text, as well as meta-data. quanteda includes tools to make it
easy and fast to manipulate the texts in a corpus, by performing the
most common natural language processing tasks simply and quickly, such
as tokenizing, stemming, or forming ngrams. quanteda's functions for
tokenizing texts and forming multiple tokenized documents into a
document-feature matrix are both extremely fast and very simple to use.
quanteda can segment texts easily by words, paragraphs, sentences, or
even user-supplied delimiters and tags.

Built on the text processing functions in the stringi package, which is
in turn built on C++ implementation of the ICU libraries for Unicode
text handling, quanteda pays special attention to fast and correct
implementation of Unicode and the handling of text in any character set.

quanteda is built for efficiency and speed, through its design around
three infrastructures: the stringi package for text processing, the
Matrix package for sparse matrix objects, and computationally intensive
processing (e.g. for tokens) handled in parallelized C++. If you can fit
it into memory, quanteda will handle it quickly. (And eventually, we
will make it possible to process objects even larger than available
memory.)

quanteda is principally designed to allow users a fast and convenient
method to go from a corpus of texts to a selected matrix of documents by
features, after defining what the documents and features. The package
makes it easy to redefine documents, for instance by splitting them into
sentences or paragraphs, or by tags, as well as to group them into
larger documents by document variables, or to subset them based on
logical conditions or combinations of document variables. The package
also implements common NLP feature selection functions, such as removing
stopwords and stemming in numerous languages, selecting words found in
dictionaries, treating words as equivalent based on a user-defined
"thesaurus", and trimming and weighting features based on document
frequency, feature frequency, and related measures such as tf-idf.

Tools for working with dictionaries are one of quanteda's principal
strengths, and the package includes several core functions for preparing
and applying dictionaries to texts, for example for lexicon-based
sentiment analysis.

Once constructed, a quanteda document-feature matrix
("[dfm](https://quanteda.io/reference/dfm.md)") can be easily analyzed
using either quanteda's built-in tools for scaling document positions,
or used with a number of other text analytic tools, such as: topic
models (including converters for direct use with the topicmodels, LDA,
and stm packages) document scaling (using the quanteda.textmodels
package's functions for the "wordfish" and "Wordscores" models, or
direct use with the **ca** package for correspondence analysis), or
machine learning through a variety of other packages that take matrix or
matrix-like inputs. quanteda includes functions for converting its core
objects, but especially a dfm, into other formats so that these are easy
to use with other analytic packages.

Additional features of quanteda include:

- powerful, flexible tools for working with
  [dictionaries](https://quanteda.io/reference/dictionary.md);

- the ability to identify
  [keywords](https://quanteda.io/reference/kwic.md) associated with
  documents or groups of documents;

- the ability to explore texts using
  [keywords-in-context](https://quanteda.io/reference/kwic.md);

- quick computation of word or document statistics, using the
  quanteda.textstats package, for clustering or to compute distances for
  other purposes;

- a comprehensive suite of [descriptive statistics on
  text](https://quanteda.io/reference/summary.corpus.md) such as the
  number of sentences, words, characters, or syllables per document; and

- flexible, easy to use graphical tools to portray many of the analyses
  available in the package.

## Source code and additional information

<https://github.com/quanteda/quanteda>

## See also

Useful links:

- <https://quanteda.io>

- Report bugs at <https://github.com/quanteda/quanteda/issues>

## Author

**Maintainer**: Kenneth Benoit <kbenoit@lse.ac.uk>
([ORCID](https://orcid.org/0000-0002-0797-564X)) \[copyright holder\]

Authors:

- Kenneth Benoit <kbenoit@lse.ac.uk>
  ([ORCID](https://orcid.org/0000-0002-0797-564X)) \[copyright holder\]

- Kohei Watanabe <watanabe.kohei@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-6519-5265))

- Haiyan Wang <whyinsa@yahoo.com>
  ([ORCID](https://orcid.org/0000-0003-4992-4311))

- Paul Nulty <paul.nulty@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-7214-4666))

- Adam Obeng <quanteda@binaryeagle.com>
  ([ORCID](https://orcid.org/0000-0002-2906-4775))

- Stefan Müller <stefan.mueller@ucd.ie>
  ([ORCID](https://orcid.org/0000-0002-6315-4125))

- Akitaka Matsuo <a.matsuo@essex.ac.uk>
  ([ORCID](https://orcid.org/0000-0002-3323-6330))

- William Lowe <lowe@hertie-school.org>
  ([ORCID](https://orcid.org/0000-0002-1549-6163))

Other contributors:

- Christian Müller <C.Mueller@lse.ac.uk> \[contributor\]

- Olivier Delmarcelle <olivier.delmarcelle@ugent.be>
  ([ORCID](https://orcid.org/0000-0003-4347-070X)) \[contributor\]

- European Research Council (ERC-2011-StG 283794-QUANTESS) \[funder\]
