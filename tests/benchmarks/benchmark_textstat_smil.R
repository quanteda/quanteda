quanteda_options(threads = 8)
#mt <- dfm(c("a b c", "a b d", "b d e", "e"))
#mt <- dfm(corpus_reshape(data_corpus_inaugural, "sentence"))
mt <- dfm(data_corpus_inaugural)

# consine ---------------

microbenchmark::microbenchmark(
    textstat_simil2(mt, margin = "features"),
    textstat_simil2(mt, margin = "features", min_simil = 0.5),
    textstat_simil2(mt, margin = "features", min_simil = 0.8),
    textstat_simil(mt, margin = "features", method = "cosine"),
    times = 10
)

cos_new1 <- textstat_simil2(mt, margin = "feature", method = "cosine")
cos_new2 <- textstat_simil2(mt, margin = "feature", method = "cosine", min_simil = 0.5)
cos_old <- textstat_simil(mt, margin = "feature", method = "cosine")

print(object.size(cos_new1), units = "MB")
print(object.size(cos_new2), units = "MB")
print(object.size(cos_old), units = "MB")

testthat::expect_equal(
    as.matrix(cos_new1),
    as.matrix(cos_old),
    tolerance = 0.001
)

# correlation ---------------

microbenchmark::microbenchmark(
    textstat_simil2(mt, margin = "features", method = "correlation"),
    textstat_simil2(mt, margin = "features", method = "correlation", min_simil = 0.5),
    textstat_simil2(mt, margin = "features", method = "correlation", min_simil = 0.8),
    textstat_simil(mt, margin = "features", method = "correlation"),
    times = 10
)

cor_new1 <- textstat_simil2(mt, margin = "feature", method = "correlation")
cor_new2 <- textstat_simil2(mt, margin = "feature", method = "correlation", min_simil = 0.5)
cor_old <- textstat_simil(mt, margin = "feature", method = "correlation")

print(object.size(cor_new1), units = "MB")
print(object.size(cor_new2), units = "MB")
print(object.size(cor_old), units = "MB")

testthat::expect_equal(
    as.matrix(cor_new1),
    as.matrix(cor_old),
    tolerance = 0.001
)