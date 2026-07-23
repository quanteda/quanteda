# Segment texts on a pattern match

Segment corpus text(s) or a character vector, splitting on a pattern
match. This is useful for breaking the texts into smaller documents
based on a regular pattern (such as a speaker identifier in a
transcript) or a user-supplied annotation.

## Usage

``` r
corpus_segment(
  x,
  pattern = "##*",
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  extract_pattern = TRUE,
  pattern_position = c("before", "after"),
  use_docvars = TRUE
)

char_segment(
  x,
  pattern = "##*",
  valuetype = c("glob", "regex", "fixed"),
  case_insensitive = TRUE,
  remove_pattern = TRUE,
  pattern_position = c("before", "after")
)
```

## Arguments

- x:

  character or [corpus](https://quanteda.io/reference/corpus.md) object
  whose texts will be segmented

- pattern:

  a character vector, list of character vectors,
  [dictionary](https://quanteda.io/reference/dictionary.md), or
  collocations object. See
  [pattern](https://quanteda.io/reference/pattern.md) for details.

- valuetype:

  the type of pattern matching: `"glob"` for "glob"-style wildcard
  expressions; `"regex"` for regular expressions; or `"fixed"` for exact
  matching. See [valuetype](https://quanteda.io/reference/valuetype.md)
  for details.

- case_insensitive:

  logical; if `TRUE`, ignore case when matching a `pattern` or
  [dictionary](https://quanteda.io/reference/dictionary.md) values

- extract_pattern:

  extracts matched patterns from the texts and save in docvars if `TRUE`

- pattern_position:

  either `"before"` or `"after"`, depending on whether the pattern
  precedes the text (as with a user-supplied tag, such as `##INTRO` in
  the examples below) or follows the text (as with punctuation
  delimiters)

- use_docvars:

  if `TRUE`, repeat the docvar values for each segmented text; if
  `FALSE`, drop the docvars in the segmented corpus. Dropping the
  docvars might be useful in order to conserve space or if these are not
  desired for the segmented corpus.

- remove_pattern:

  removes matched patterns from the texts if `TRUE`

## Value

`corpus_segment` returns a corpus of segmented texts

`char_segment` returns a character vector of segmented texts

## Details

For segmentation into syntactic units defined by the locale (such as
sentences), use
[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
instead. In cases where more fine-grained segmentation is needed, such
as that based on commas or semi-colons (phrase delimiters within a
sentence), `corpus_segment()` offers greater user control than
[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md).

## Boundaries and segmentation explained

The `pattern` acts as a boundary delimiter that defines the segmentation
points for splitting a text into new "document" units. Boundaries are
always defined as the pattern matches, plus the end and beginnings of
each document. The new "documents" that are created following the
segmentation will then be the texts found between boundaries.

The pattern itself will be saved as a new document variable named
`pattern`. This is most useful when segmenting a text according to tags
such as names in a transcript, section titles, or user-supplied
annotations. If the beginning of the file precedes a pattern match, then
the extracted text will have a `NA` for the extracted `pattern` document
variable (or when `pattern_position = "after"`, this will be true for
the text split between the last pattern match and the end of the
document).

To extract syntactically defined sub-document units such as sentences
and paragraphs, use
[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md)
instead.

## Using patterns

One of the most common uses for `corpus_segment` is to partition a
corpus into sub-documents using tags. The default pattern value is
designed for a user-annotated tag that is a term beginning with double
"hash" signs, followed by a whitespace, for instance as
`##INTRODUCTION The text`.

Glob and fixed pattern types use a whitespace character to signal the
end of the pattern.

For more advanced pattern matches that could include whitespace or
newlines, a regex pattern type can be used, for instance a text such as

`Mr. Smith: Text`\
`Mrs. Jones: More text`

could have as `pattern = "\\b[A-Z].+\\.\\s[A-Z][a-z]+:"`, which would
catch the title, the name, and the colon.

For custom boundary delimitation using punctuation characters that come
come at the end of a clause or sentence (such as `,` and`.`, these can
be specified manually and `pattern_position` set to `"after"`. To keep
the punctuation characters in the text (as with sentence segmentation),
set `extract_pattern = FALSE`. (With most tag applications, users will
want to remove the patterns from the text, as they are annotations
rather than parts of the text itself.)

## See also

[`corpus_reshape()`](https://quanteda.io/reference/corpus_reshape.md),
for segmenting texts into pre-defined syntactic units such as sentences,
paragraphs, or fixed-length chunks

## Examples

``` r
## segmenting a corpus

# segmenting a corpus using tags
corp1 <- corpus(c("##INTRO This is the introduction.
                  ##DOC1 This is the first document.  Second sentence in Doc 1.
                  ##DOC3 Third document starts here.  End of third document.",
                 "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
corpseg1 <- corpus_segment(corp1, pattern = "##*")
cbind(corpseg1, docvars(corpseg1))
#>                                                       corpseg1  pattern
#> text1.1                              This is the introduction.  ##INTRO
#> text1.2 This is the first document.  Second sentence in Doc 1.   ##DOC1
#> text1.3    Third document starts here.  End of third document.   ##DOC3
#> text2.1                                               Document  ##INTRO
#> text2.2                                      Two starts before ##NUMBER
#> text2.3                                                 Three. ##NUMBER

# segmenting a transcript based on speaker identifiers
corp2 <- corpus("Mr. Smith: Text.\nMrs. Jones: More text.\nMr. Smith: I'm speaking, again.")
corpseg2 <- corpus_segment(corp2, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:",
                           valuetype = "regex")
cbind(corpseg2, docvars(corpseg2))
#>                     corpseg2     pattern
#> text1.1                Text.  Mr. Smith:
#> text1.2           More text. Mrs. Jones:
#> text1.3 I'm speaking, again.  Mr. Smith:

# segmenting a corpus using crude end-of-sentence segmentation
corpseg3 <- corpus_segment(corp1, pattern = ".", valuetype = "fixed",
                           pattern_position = "after", extract_pattern = FALSE)
cbind(corpseg3, docvars(corpseg3))
#>                                                            corpseg3
#> text1.1                           ##INTRO This is the introduction.
#> text1.2                          ##DOC1 This is the first document.
#> text1.3                                   Second sentence in Doc 1.
#> text1.4                          ##DOC3 Third document starts here.
#> text1.5                                      End of third document.
#> text2.1 ##INTRO Document ##NUMBER Two starts before ##NUMBER Three.

## segmenting a character vector

# segment into paragraphs and removing the "- " bullet points
cat(data_char_ukimmig2010[4])
#> Immigration.
#> 
#> Migration is a fact of life.  People have always moved from one country to another, and as a practical matter the ability to control borders without oppressive measures is more limited than most politicians like to pretend. Much of our language, culture and way of life have been enriched by successive new arrivals over two thousand years. It is not just a matter of immigration: over 5 million British Citizens benefit from other countries' liberal immigration policies by living abroad.
#> 
#> The causes of a person moving to the UK are complex. For the person concerned, there may be escape from persecution and improved economic prospects, but also separation from home, friends and family. For the country of origin, there may be the loss of skilled workers, especially health professionals, but also the receipt of remittances from the immigrant, and many migrants return with improved skills. For the community that receives the immigrant there may be the benefits of getting done jobs that no one in that community wants to or can do, more taxes being paid and the creation of a more cosmopolitan atmosphere. But there may also be costs in terms of unwelcome competition for jobs, pressure on housing and other resources and longer-term pressures on overall population.
#> 
#> In deciding policy on immigration it is important that all these points are considered and balanced against each other. We must accept too our legal and moral obligations to give sanctuary to those fleeing persecution, and the principle of free movement throughout the European Union. Against this background our policy is as follows: 
#> 
#> - Where we are limiting numbers, our priority must be to meet our obligations to refugees and those seeking sanctuary, including the increasing numbers of people displaced by environmental change, above the needs of our economy.
#> 
#> - Our immigration policies must be fair and non-discriminatory, respect the integrity of families and be applied promptly and effectively.
#> 
#> - Our international policies should every- where seek to reduce the economic, political and environmental factors that force people to migrate. Emigration should be a positive choice, not the outcome of desperation. In particular, free movement within the EU is a fact. We should press for EU policies that make all parts of the EU an attractive place to live.
#> 
#> - We reject the use of immigration as a political issue to mask problems such as a lack of high-quality social housing. The proper solution is to provide enough social housing, as we propose elsewhere in this manifesto.
#> 
#> - We should not tolerate the long-term presence of large numbers of people whose immigration status is not defined. Such immigrants are vulnerable to exploitation by unscrupulous employers and others, under- mining national terms and conditions of employment. We would open up ways for existing illegal migrants who have been here for three years to become legal. In particular, a legal status must be provided for people who have not succeeded in their claim for humanitarian protection but who cannot be returned to their country of origin due to the political situation there.
#> 
#> - We would review the asylum procedures to ensure that destitution plays no role in the asylum process by allowing those seeking sanctuary to work.
#> 
#> - We would review the Nationality, Immigration and Asylum Act 2002, particularly with regard to issues of access to legal advice, childcare and levels of subsistence allowance.
#> 
#> - Those who have been trafficked should not be subject to summary deportation. They should receive a temporary right to stay and have the same right to apply to remain as others seeking to migrate.
#> 
#> - Those seeking sanctuary should not be detained, and in particular the administrative detention of children is unacceptable and should cease immediately.
char_segment(data_char_ukimmig2010[4],
             pattern = "\\n\\n(-\\s){0,1}", valuetype = "regex",
             remove_pattern = TRUE)
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.1 
#>                                                                                                                                                                                                                                                                                                      "Migration is a fact of life.  People have always moved from one country to another, and as a practical matter the ability to control borders without oppressive measures is more limited than most politicians like to pretend. Much of our language, culture and way of life have been enriched by successive new arrivals over two thousand years. It is not just a matter of immigration: over 5 million British Citizens benefit from other countries' liberal immigration policies by living abroad." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.2 
#> "The causes of a person moving to the UK are complex. For the person concerned, there may be escape from persecution and improved economic prospects, but also separation from home, friends and family. For the country of origin, there may be the loss of skilled workers, especially health professionals, but also the receipt of remittances from the immigrant, and many migrants return with improved skills. For the community that receives the immigrant there may be the benefits of getting done jobs that no one in that community wants to or can do, more taxes being paid and the creation of a more cosmopolitan atmosphere. But there may also be costs in terms of unwelcome competition for jobs, pressure on housing and other resources and longer-term pressures on overall population." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.3 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "In deciding policy on immigration it is important that all these points are considered and balanced against each other. We must accept too our legal and moral obligations to give sanctuary to those fleeing persecution, and the principle of free movement throughout the European Union. Against this background our policy is as follows:" 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.4 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Where we are limiting numbers, our priority must be to meet our obligations to refugees and those seeking sanctuary, including the increasing numbers of people displaced by environmental change, above the needs of our economy." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.5 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "Our immigration policies must be fair and non-discriminatory, respect the integrity of families and be applied promptly and effectively." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.6 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                         "Our international policies should every- where seek to reduce the economic, political and environmental factors that force people to migrate. Emigration should be a positive choice, not the outcome of desperation. In particular, free movement within the EU is a fact. We should press for EU policies that make all parts of the EU an attractive place to live." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.7 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "We reject the use of immigration as a political issue to mask problems such as a lack of high-quality social housing. The proper solution is to provide enough social housing, as we propose elsewhere in this manifesto." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.8 
#>                                                                                                                                                                                                              "We should not tolerate the long-term presence of large numbers of people whose immigration status is not defined. Such immigrants are vulnerable to exploitation by unscrupulous employers and others, under- mining national terms and conditions of employment. We would open up ways for existing illegal migrants who have been here for three years to become legal. In particular, a legal status must be provided for people who have not succeeded in their claim for humanitarian protection but who cannot be returned to their country of origin due to the political situation there." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Greens.9 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "We would review the asylum procedures to ensure that destitution plays no role in the asylum process by allowing those seeking sanctuary to work." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Greens.10 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "We would review the Nationality, Immigration and Asylum Act 2002, particularly with regard to issues of access to legal advice, childcare and levels of subsistence allowance." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Greens.11 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Those who have been trafficked should not be subject to summary deportation. They should receive a temporary right to stay and have the same right to apply to remain as others seeking to migrate." 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Greens.12 
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "Those seeking sanctuary should not be detained, and in particular the administrative detention of children is unacceptable and should cease immediately." 

# segment a text into clauses
txt <- c(d1 = "This, is a sentence?  You: come here.", d2 = "Yes, yes okay.")
char_segment(txt, pattern = "\\p{P}", valuetype = "regex",
             pattern_position = "after", remove_pattern = FALSE)
#>             d1.1             d1.2             d1.3             d1.4 
#>          "This," "is a sentence?"           "You:"     "come here." 
#>             d2.1             d2.2 
#>           "Yes,"      "yes okay." 
```
