# Segment a corpus into chunks of a given size

Segment a corpus into new documents of roughly equal sized text chunks,
with the possibility of overlapping the chunks.

## Usage

``` r
corpus_chunk(
  x,
  size,
  truncate = FALSE,
  use_docvars = TRUE,
  verbose = quanteda_options("verbose")
)
```

## Arguments

- x:

  [tokens](https://quanteda.io/reference/tokens.md) object whose token
  elements will be segmented into chunks

- size:

  integer; the (approximate) token length of the chunks. See Details.

- truncate:

  logical; if `TRUE`, truncate the text after `size`

- use_docvars:

  if `TRUE`, repeat the docvar values for each chunk; if `FALSE`, drop
  the docvars in the chunked tokens

- verbose:

  if `TRUE` print the number of tokens and documents before and after
  the function is applied. The number of tokens does not include
  paddings.

## Details

The token length is estimated using
`stringi::stri_length(txt) / stringi::stri_count_boundaries(txt)` to
avoid needing to tokenize and rejoin the corpus from the tokens.

Note that when used for chunking texts prior to sending to large
language models (LLMs) with limited input token lengths, size should
typically be set to approximately 0.75-0.80 of the LLM's token limit.
This is because tokenizers (such as LLaMA's SentencePiece Byte-Pair
Encoding tokenizer) require more tokens than the linguistically defined
grammatically-based tokenizer that is the quanteda default. Note also
that because of the use of `stringi::stri_count_boundaries(txt)` to
approximate token length (efficiently), the exact token length for
chunking will be approximate.

## See also

[`tokens_chunk()`](https://quanteda.io/reference/tokens_chunk.md)

## Examples

``` r
data_corpus_inaugural[1] |>
  corpus_chunk(size = 10)
#> Corpus consisting of 152 documents and 4 docvars.
#> 1789-Washington.1 :
#> "Fellow-Citizens of the Senate and of the House of"
#> 
#> 1789-Washington.2 :
#> "Representatives: Among the vicissitudes incident to life"
#> 
#> 1789-Washington.3 :
#> "no event could have filled me with greater anxieties than"
#> 
#> 1789-Washington.4 :
#> "that of which the notification was transmitted by your"
#> 
#> 1789-Washington.5 :
#> "order, and received on the 14th day of the present month."
#> 
#> 1789-Washington.6 :
#> "On the one hand, I was summoned by my Country, whose voice"
#> 
#> [ reached max_ndoc ... 146 more documents ]
```
