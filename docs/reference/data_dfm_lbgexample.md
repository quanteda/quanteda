# dfm from data in Table 1 of Laver, Benoit, and Garry (2003)

Constructed example data to demonstrate the Wordscores algorithm, from
Laver Benoit and Garry (2003), Table 1.

## Usage

``` r
data(data_dfm_lbgexample)
```

## Format

A [dfm](https://quanteda.io/reference/dfm.md) object with 6 documents
and 37 features.

## Details

This is the example word count data from Laver, Benoit and Garry's
(2003) Table 1. Documents R1 to R5 are assumed to have known positions:
-1.5, -0.75, 0, 0.75, 1.5. Document V1 is assumed unknown, and will have
a raw text score of approximately -0.45 when computed as per LBG (2003).

## References

Laver, M., Benoit, K.R., & Garry, J. (2003). [Estimating Policy
Positions from Political Text using Words as
Data](https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf). *American
Political Science Review*, 97(2), 311–331.
