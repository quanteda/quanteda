# if this test fails, then correct the misspelled words, or add to 
# inst/WORDLIST if they are exceptions.
# devtools::spell_check() is a good way to find them.

if (requireNamespace("spelling", quietly = TRUE))
    spelling::spell_check_test(vignettes = TRUE, error = TRUE, skip_on_cran = TRUE)
