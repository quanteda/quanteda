context("test textmodel_NB")

## Example from 13.1 of _An Introduction to Information Retrieval_
txt <- c(d1 = "Chinese Beijing Chinese",
         d2 = "Chinese Chinese Shanghai",
         d3 = "Chinese Macao",
         d4 = "Tokyo Japan Chinese",
         d5 = "Chinese Chinese Chinese Tokyo Japan")
trainingset <- dfm(txt, tolower = FALSE)
trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)

nb_multi_smooth <- 
    textmodel_NB(trainingset, trainingclass, prior = "docfreq", distribution = "multinomial", smooth = 1)
nb_multi_nosmooth <- 
    textmodel_NB(trainingset, trainingclass, prior = "docfreq", distribution = "multinomial", smooth = 0)
nb_bern_smooth <- 
    textmodel_NB(trainingset, trainingclass, prior = "docfreq", distribution = "Bernoulli", smooth = 1)
nb_bern_nosmooth <- 
    textmodel_NB(trainingset, trainingclass, prior = "docfreq", distribution = "Bernoulli", smooth = 0)

test_that("class priors are preserved in correct order", {
    expect_equal(textmodel_NB(trainingset, trainingclass, prior = "uniform")$Pc,
                 c(Y = 0.5, N = 0.5))
    expect_equal(textmodel_NB(trainingset, trainingclass, prior = "docfreq")$Pc,
                 c(Y = 0.75, N = 0.25))
    expect_equal(round(textmodel_NB(trainingset, trainingclass, prior = "termfreq")$Pc, 2),
                 c(Y = 0.73, N = 0.27))
})


test_that("bernoulli diff from multinomial model (#776)", {
    expect_true(
        !identical(nb_multi_smooth$PcGw[1,], nb_bern_smooth$PcGw[1,])
    )
})

test_that("multinomial likelihoods and class posteriors are correct", {
    # test for results from p261, https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf
    
    # with smoothing
    expect_identical(nb_multi_smooth$PwGc["Y", "Chinese"], 3/7)
    expect_identical(nb_multi_smooth$PwGc["Y", "Tokyo"], 1/14)
    expect_identical(nb_multi_smooth$PwGc["Y", "Japan"], 1/14)
    expect_identical(nb_multi_smooth$PwGc["N", "Chinese"], 2/9)
    expect_identical(nb_multi_smooth$PwGc["N", "Tokyo"], 2/9)
    expect_identical(nb_multi_smooth$PwGc["N", "Japan"], 2/9)
    
    # without smoothing
    expect_identical(nb_multi_nosmooth$PwGc["Y", "Chinese"], 5/8)
    expect_identical(nb_multi_nosmooth$PwGc["Y", "Tokyo"], 0/8)
    expect_identical(nb_multi_nosmooth$PwGc["Y", "Japan"], 0/8)
    expect_identical(nb_multi_nosmooth$PwGc["N", "Chinese"], 1/3)
    expect_identical(nb_multi_nosmooth$PwGc["N", "Tokyo"], 1/3)
    expect_identical(nb_multi_nosmooth$PwGc["N", "Japan"], 1/3)
})

test_that("Bernoulli likelihoods and class posteriors are correct", {
    # test for results from p261, https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf
    
    # with smoothing
    expect_identical(nb_bern_smooth$PwGc["Y", "Chinese"], 4/5)
    expect_identical(nb_bern_smooth$PwGc["Y", "Japan"], 1/5)
    expect_identical(nb_bern_smooth$PwGc["Y", "Tokyo"], 1/5)
    expect_identical(nb_bern_smooth$PwGc["Y", "Beijing"], 2/5)
    expect_identical(nb_bern_smooth$PwGc["Y", "Macao"], 2/5)
    expect_identical(nb_bern_smooth$PwGc["Y", "Shanghai"], 2/5)
    expect_identical(nb_bern_smooth$PwGc["N", "Chinese"], 2/3)
    expect_identical(nb_bern_smooth$PwGc["N", "Japan"], 2/3)
    expect_identical(nb_bern_smooth$PwGc["N", "Tokyo"], 2/3)
    expect_identical(nb_bern_smooth$PwGc["N", "Beijing"], 1/3)
    expect_identical(nb_bern_smooth$PwGc["N", "Macao"], 1/3)
    expect_identical(nb_bern_smooth$PwGc["N", "Shanghai"], 1/3)
    
    # without smoothing
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Chinese"], 3/3)
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Japan"], 0/3)
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Tokyo"], 0/3)
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Beijing"], 1/3)
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Macao"], 1/3)
    expect_identical(nb_bern_nosmooth$PwGc["Y", "Shanghai"], 1/3)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Chinese"], 1/1)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Japan"], 1/1)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Tokyo"], 1/1)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Beijing"], 0/1)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Macao"], 0/1)
    expect_identical(nb_bern_nosmooth$PwGc["N", "Shanghai"], 0/1)
})

test_that("Bernoulli NB predicted values are correct", {
    book_lik_Y <- 3/4 * 4/5 * 1/5 * 1/5 * (1-2/5) * (1-2/5) * (1-2/5)  # 0.005184 
    book_lik_N <- 1/4 * 2/3 * 2/3 * 2/3 * (1-1/3) * (1-1/3) * (1-1/3)  # 0.02194787
    nb_bern_smooth_pred <- predict(nb_bern_smooth)
    expect_equal( 
        book_lik_Y / (book_lik_Y + book_lik_N),
        nb_bern_smooth_pred$posterior.prob["d5", "Y"]
    )
    expect_equal( 
        book_lik_N / (book_lik_Y + book_lik_N),
        nb_bern_smooth_pred$posterior.prob["d5", "N"]
    )
})


