# Package index

## Package-level

- [`quanteda-package`](https://quanteda.io/reference/quanteda-package.md)
  [`quanteda`](https://quanteda.io/reference/quanteda-package.md) : An R
  package for the quantitative analysis of textual data
- [`quanteda_options()`](https://quanteda.io/reference/quanteda_options.md)
  : Get or set package options for quanteda

## Data

Built-in data objects.

- [`data_char_sampletext`](https://quanteda.io/reference/data_char_sampletext.md)
  : A paragraph of text for testing various text-based functions
- [`data_char_ukimmig2010`](https://quanteda.io/reference/data_char_ukimmig2010.md)
  : Immigration-related sections of 2010 UK party manifestos
- [`data_corpus_inaugural`](https://quanteda.io/reference/data_corpus_inaugural.md)
  : US presidential inaugural address texts
- [`data_dfm_lbgexample`](https://quanteda.io/reference/data_dfm_lbgexample.md)
  : dfm from data in Table 1 of Laver, Benoit, and Garry (2003)
- [`data_dictionary_LSD2015`](https://quanteda.io/reference/data_dictionary_LSD2015.md)
  : Lexicoder Sentiment Dictionary (2015)
- [`data-relocated`](https://quanteda.io/reference/data-relocated.md)
  [`data_corpus_dailnoconf1991`](https://quanteda.io/reference/data-relocated.md)
  [`data_corpus_irishbudget2010`](https://quanteda.io/reference/data-relocated.md)
  : Formerly included data objects

## Corpus functions

Functions for constructing and manipulating corpus class objects.

- [`corpus()`](https://quanteda.io/reference/corpus.md) : Construct a
  corpus object
- [`corpus_chunk()`](https://quanteda.io/reference/corpus_chunk.md) :
  Segment a corpus into chunks of a given size
- [`corpus_group()`](https://quanteda.io/reference/corpus_group.md) :
  Combine documents in corpus by a grouping variable
- [`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
  : Recast the document units of a corpus
- [`corpus_sample()`](https://quanteda.io/reference/corpus_sample.md) :
  Randomly sample documents from a corpus
- [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)
  [`char_segment()`](https://quanteda.io/reference/corpus_segment.md) :
  Segment texts on a pattern match
- [`corpus_subset()`](https://quanteda.io/reference/corpus_subset.md) :
  Extract a subset of a corpus
- [`corpus_trim()`](https://quanteda.io/reference/corpus_trim.md)
  [`char_trim()`](https://quanteda.io/reference/corpus_trim.md) : Remove
  sentences based on their token lengths or a pattern match
- [`docvars()`](https://quanteda.io/reference/docvars.md)
  [`` `docvars<-`() ``](https://quanteda.io/reference/docvars.md)
  [`` `$`( ``*`<corpus>`*`)`](https://quanteda.io/reference/docvars.md)
  [`` `$<-`( ``*`<corpus>`*`)`](https://quanteda.io/reference/docvars.md)
  [`` `$`( ``*`<tokens>`*`)`](https://quanteda.io/reference/docvars.md)
  [`` `$<-`( ``*`<tokens>`*`)`](https://quanteda.io/reference/docvars.md)
  [`` `$`( ``*`<dfm>`*`)`](https://quanteda.io/reference/docvars.md)
  [`` `$<-`( ``*`<dfm>`*`)`](https://quanteda.io/reference/docvars.md) :
  Get or set document-level variables
- [`as.character(`*`<corpus>`*`)`](https://quanteda.io/reference/as.character.corpus.md)
  [`is.corpus()`](https://quanteda.io/reference/as.character.corpus.md)
  [`as.corpus()`](https://quanteda.io/reference/as.character.corpus.md)
  : Coercion and checking methods for corpus objects

## Tokens functions

Functions for constructing and manipulating tokens class objects.

- [`tokens()`](https://quanteda.io/reference/tokens.md) : Construct a
  tokens object
- [`tokens_annotate()`](https://quanteda.io/reference/tokens_annotate.md)
  : Annotate a tokens object using a dictionary
- [`tokens_chunk()`](https://quanteda.io/reference/tokens_chunk.md) :
  Segment tokens object by chunks of a given size
- [`tokens_compound()`](https://quanteda.io/reference/tokens_compound.md)
  : Convert token sequences into compound tokens
- [`tokens_group()`](https://quanteda.io/reference/tokens_group.md) :
  Combine documents in a tokens object by a grouping variable
- [`tokens_lookup()`](https://quanteda.io/reference/tokens_lookup.md) :
  Apply a dictionary to a tokens object
- [`tokens_match()`](https://quanteda.io/reference/tokens_match.md) :
  Match the tokens IDs with given types
- [`tokens_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  [`char_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  [`tokens_skipgrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  : Create n-grams and skip-grams from tokens
- [`tokens_replace()`](https://quanteda.io/reference/tokens_replace.md)
  : Replace tokens in a tokens object
- [`tokens_sample()`](https://quanteda.io/reference/tokens_sample.md) :
  Randomly sample documents from a tokens object
- [`tokens_segment()`](https://quanteda.io/reference/tokens_segment.md)
  : Segment tokens object by patterns
- [`tokens_select()`](https://quanteda.io/reference/tokens_select.md)
  [`tokens_remove()`](https://quanteda.io/reference/tokens_select.md)
  [`tokens_keep()`](https://quanteda.io/reference/tokens_select.md) :
  Select or remove tokens from a tokens object
- [`tokens_split()`](https://quanteda.io/reference/tokens_split.md) :
  Split tokens by a separator pattern
- [`tokens_subset()`](https://quanteda.io/reference/tokens_subset.md) :
  Extract a subset of a tokens
- [`tokens_tolower()`](https://quanteda.io/reference/tokens_tolower.md)
  [`tokens_toupper()`](https://quanteda.io/reference/tokens_tolower.md)
  : Convert the case of tokens
- [`tokens_trim()`](https://quanteda.io/reference/tokens_trim.md) : Trim
  tokens using frequency threshold-based feature selection
- [`tokens_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`char_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md) :
  Stem the terms in an object
- [`is.tokens_xptr()`](https://quanteda.io/reference/tokens_xptr.md)
  [`as.tokens_xptr()`](https://quanteda.io/reference/tokens_xptr.md) :
  Methods for tokens_xptr objects
- [`types()`](https://quanteda.io/reference/types.md) : Get word types
  from a tokens object
- [`concat()`](https://quanteda.io/reference/concat.md)
  [`concatenator()`](https://quanteda.io/reference/concat.md) : Return
  the concatenator character from an object
- [`as.list(`*`<tokens>`*`)`](https://quanteda.io/reference/as.tokens.md)
  [`as.character(`*`<tokens>`*`)`](https://quanteda.io/reference/as.tokens.md)
  [`is.tokens()`](https://quanteda.io/reference/as.tokens.md)
  [`as.tensor()`](https://quanteda.io/reference/as.tokens.md)
  [`as.matrix(`*`<tokens>`*`)`](https://quanteda.io/reference/as.tokens.md)
  [`as.tokens()`](https://quanteda.io/reference/as.tokens.md) :
  Coercion, checking, and combining functions for tokens objects

## Character functions

Functions for constructing and manipulating character objects.

- [`char_tolower()`](https://quanteda.io/reference/char_tolower.md)
  [`char_toupper()`](https://quanteda.io/reference/char_tolower.md) :
  Convert the case of character objects
- [`corpus_segment()`](https://quanteda.io/reference/corpus_segment.md)
  [`char_segment()`](https://quanteda.io/reference/corpus_segment.md) :
  Segment texts on a pattern match
- [`tokens_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  [`char_ngrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  [`tokens_skipgrams()`](https://quanteda.io/reference/tokens_ngrams.md)
  : Create n-grams and skip-grams from tokens
- [`char_select()`](https://quanteda.io/reference/char_select.md)
  [`char_remove()`](https://quanteda.io/reference/char_select.md)
  [`char_keep()`](https://quanteda.io/reference/char_select.md) : Select
  or remove elements from a character vector
- [`corpus_trim()`](https://quanteda.io/reference/corpus_trim.md)
  [`char_trim()`](https://quanteda.io/reference/corpus_trim.md) : Remove
  sentences based on their token lengths or a pattern match
- [`tokens_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`char_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md) :
  Stem the terms in an object

## Text matrix functions

Functions for constructing and manipulating a document-feature matrix
(dfm) or feature co-occurrence matrix object.

- [`dfm()`](https://quanteda.io/reference/dfm.md) : Create a
  document-feature matrix

- [`dfm_compress()`](https://quanteda.io/reference/dfm_compress.md)
  [`fcm_compress()`](https://quanteda.io/reference/dfm_compress.md) :
  Recombine a dfm or fcm by combining identical dimension elements

- [`dfm_group()`](https://quanteda.io/reference/dfm_group.md) : Combine
  documents in a dfm by a grouping variable

- [`dfm_lookup()`](https://quanteda.io/reference/dfm_lookup.md) : Apply
  a dictionary to a dfm

- [`dfm_match()`](https://quanteda.io/reference/dfm_match.md) : Match
  the dfm columns with given features

- [`dfm_replace()`](https://quanteda.io/reference/dfm_replace.md) :
  Replace features in dfm

- [`dfm_sample()`](https://quanteda.io/reference/dfm_sample.md) :
  Randomly sample documents from a dfm

- [`dfm_select()`](https://quanteda.io/reference/dfm_select.md)
  [`dfm_remove()`](https://quanteda.io/reference/dfm_select.md)
  [`dfm_keep()`](https://quanteda.io/reference/dfm_select.md)
  [`fcm_select()`](https://quanteda.io/reference/dfm_select.md)
  [`fcm_remove()`](https://quanteda.io/reference/dfm_select.md)
  [`fcm_keep()`](https://quanteda.io/reference/dfm_select.md) : Select
  features from a dfm or fcm

- [`dfm_sort()`](https://quanteda.io/reference/dfm_sort.md) : Sort a dfm
  by frequency of one or more margins

- [`dfm_subset()`](https://quanteda.io/reference/dfm_subset.md) :
  Extract a subset of a dfm

- [`dfm_tfidf()`](https://quanteda.io/reference/dfm_tfidf.md) :

  Weight a dfm by *tf-idf*

- [`dfm_tolower()`](https://quanteda.io/reference/dfm_tolower.md)
  [`dfm_toupper()`](https://quanteda.io/reference/dfm_tolower.md)
  [`fcm_tolower()`](https://quanteda.io/reference/dfm_tolower.md)
  [`fcm_toupper()`](https://quanteda.io/reference/dfm_tolower.md) :
  Convert the case of the features of a dfm and combine

- [`dfm_trim()`](https://quanteda.io/reference/dfm_trim.md) : Trim a dfm
  using frequency threshold-based feature selection

- [`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md)
  [`dfm_smooth()`](https://quanteda.io/reference/dfm_weight.md) : Weight
  the feature frequencies in a dfm

- [`tokens_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`char_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md)
  [`dfm_wordstem()`](https://quanteda.io/reference/tokens_wordstem.md) :
  Stem the terms in an object

- [`docfreq()`](https://quanteda.io/reference/docfreq.md) : Compute the
  (weighted) document frequency of a feature

- [`featfreq()`](https://quanteda.io/reference/featfreq.md) : Compute
  the frequencies of features

- [`head(`*`<dfm>`*`)`](https://quanteda.io/reference/head.dfm.md)
  [`tail(`*`<dfm>`*`)`](https://quanteda.io/reference/head.dfm.md) :
  Return the first or last part of a dfm

- [`as.dfm()`](https://quanteda.io/reference/as.dfm.md)
  [`is.dfm()`](https://quanteda.io/reference/as.dfm.md) : Coercion and
  checking functions for dfm objects

- [`as.matrix(`*`<dfm>`*`)`](https://quanteda.io/reference/as.matrix.dfm.md)
  : Coerce a dfm to a matrix or data.frame

- [`fcm()`](https://quanteda.io/reference/fcm.md) : Create a feature
  co-occurrence matrix

- [`fcm_sort()`](https://quanteda.io/reference/fcm_sort.md) : Sort an
  fcm in alphabetical order of the features

- [`as.fcm()`](https://quanteda.io/reference/as.fcm.md) : Coercion and
  checking functions for fcm objects

## Dictionary functions

Constructor and utility functions for working with dictionaries.

- [`dictionary()`](https://quanteda.io/reference/dictionary.md) : Create
  a dictionary object
- [`as.dictionary()`](https://quanteda.io/reference/as.dictionary.md)
  [`is.dictionary()`](https://quanteda.io/reference/as.dictionary.md) :
  Coercion and checking functions for dictionary objects
- [`as.yaml()`](https://quanteda.io/reference/as.yaml.md) : Convert
  quanteda dictionary objects to the YAML format

## Phrase discovery functions

Functions for exploring and detecting keywords and phrases.

- [`is.collocations()`](https://quanteda.io/reference/is.collocations.md)
  : Check if an object is collocations
- [`kwic()`](https://quanteda.io/reference/kwic.md)
  [`is.kwic()`](https://quanteda.io/reference/kwic.md)
  [`as.data.frame(`*`<kwic>`*`)`](https://quanteda.io/reference/kwic.md)
  : Locate keywords-in-context

## Utility functions

R-like functions to return counts and object information.

- [`index()`](https://quanteda.io/reference/index-topic.md)
  [`is.index()`](https://quanteda.io/reference/index-topic.md) : Locate
  a pattern in a tokens object
- [`ndoc()`](https://quanteda.io/reference/ndoc.md)
  [`nfeat()`](https://quanteda.io/reference/ndoc.md) : Count the number
  of documents or features
- [`nsentence()`](https://quanteda.io/reference/nsentence.md)
  **\[deprecated\]** : Count the number of sentences
- [`ntoken()`](https://quanteda.io/reference/ntoken.md)
  [`ntype()`](https://quanteda.io/reference/ntoken.md) : Count the
  number of tokens or types
- [`print(`*`<corpus>`*`)`](https://quanteda.io/reference/print-methods.md)
  [`print(`*`<dfm>`*`)`](https://quanteda.io/reference/print-methods.md)
  [`print(`*`<dictionary2>`*`)`](https://quanteda.io/reference/print-methods.md)
  [`print(`*`<fcm>`*`)`](https://quanteda.io/reference/print-methods.md)
  [`print(`*`<kwic>`*`)`](https://quanteda.io/reference/print-methods.md)
  [`print(`*`<tokens>`*`)`](https://quanteda.io/reference/print-methods.md)
  : Print methods for quanteda core objects
- [`docnames()`](https://quanteda.io/reference/docnames.md)
  [`` `docnames<-`() ``](https://quanteda.io/reference/docnames.md)
  [`docid()`](https://quanteda.io/reference/docnames.md)
  [`segid()`](https://quanteda.io/reference/docnames.md) : Get or set
  document names
- [`featnames()`](https://quanteda.io/reference/featnames.md) : Get the
  feature labels from a dfm

## Miscellaneous functions

- [`phrase()`](https://quanteda.io/reference/phrase.md)
  [`as.phrase()`](https://quanteda.io/reference/phrase.md)
  [`is.phrase()`](https://quanteda.io/reference/phrase.md) : Declare a
  pattern to be a sequence of separate patterns
- [`convert()`](https://quanteda.io/reference/convert.md) : Convert
  quanteda objects to non-quanteda formats
- [`bootstrap_dfm()`](https://quanteda.io/reference/bootstrap_dfm.md) :
  Bootstrap a dfm
- [`meta()`](https://quanteda.io/reference/meta.md)
  [`` `meta<-`() ``](https://quanteda.io/reference/meta.md) : Get or set
  object metadata
- [`spacyr-methods`](https://quanteda.io/reference/spacyr-methods.md) :
  Extensions for and from spacy_parse objects

## Statistics, models, and plots

Functions for computing statistics, fitting models, and producing
visualisations models from text.

- [`sparsity()`](https://quanteda.io/reference/sparsity.md) : Compute
  the sparsity of a document-feature matrix
- [`topfeatures()`](https://quanteda.io/reference/topfeatures.md) :
  Identify the most frequent features in a dfm
- [`textmodels`](https://quanteda.io/reference/textmodels.md) : Models
  for scaling and classification of textual data
- [`textplots`](https://quanteda.io/reference/textplots.md) : Plots for
  textual data
- [`textstats`](https://quanteda.io/reference/textstats.md) : Statistics
  for textual data
