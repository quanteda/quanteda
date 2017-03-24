context("test textmodel_wordshoal")

test_that("textmodel_wordshoal is functional", {
    skip_on_cran()
    skip_on_appveyor()
    skip_on_travis()
    skip_on_os("mac")
    skip_on_os("linux")
    skip_on_os("windows")
    skip_on_os("solaris")
    skip_if_not_installed("quantedaData")
    data(data_corpus_irish30, package = "quantedaData")
    # data_corpus_irish30 <- 
    #     corpus_subset(data_corpus_irish30, party.name %in% c("Labour"))
    iedfm <- dfm(data_corpus_irish30, removePunct = TRUE, removeTwitter = TRUE)
    expect_output(
        wordshoalfit <-
            textmodel_wordshoal(iedfm,
                                groups = docvars(data_corpus_irish30, "debateID"),
                                authors = docvars(data_corpus_irish30, "member.name")),
        "Elapsed time"
    )
})
