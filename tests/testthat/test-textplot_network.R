context("test textplot_network.R")

test_that("test textplot_network", {
    #skip_on_os("linux")
    txt <- "A D A C E A D F E B A C E D"
    testfcm <- fcm(txt, context = "window", window = 3, tri = FALSE)
    testdfm <- dfm(txt)
    expect_silent(textplot_network(testfcm, vertex_color = 'red', offset = 0.1))
    expect_silent(textplot_network(testdfm, offset = 0.1))
    expect_error(textplot_network(testfcm, min_freq = 100), 
                 'There is no co-occurence higher than the threshold')
    
    # works with interger color
    expect_silent(textplot_network(testfcm, vertex_color = 2))
    expect_silent(textplot_network(testfcm, edge_color = 2))
})


test_that("test textplot_network works with vectorlized argument", {
    #skip_on_os("linux")
    txt <- "A D A C E A D F E B A C E D"
    
    testfcm <- fcm(txt, context = "window", window = 3, tri = FALSE)
    expect_silent(textplot_network(testfcm, vertex_color = rep(c(1, 2), nrow(testfcm) / 2)))
    expect_silent(textplot_network(testfcm, vertex_size = rowSums(testfcm) / 5))
    expect_silent(textplot_network(testfcm, vertex_labelcolor = rep(c(1, NA), nrow(testfcm) / 2)))
    expect_silent(textplot_network(testfcm, vertex_labelsize = rowSums(testfcm) / 5))
})

test_that("textplot_network error when fcm is too large", {
    testdfm <- fcm(data_corpus_inaugural[1:5])
    expect_error(textplot_network(testdfm, min_freq = 1, offset = 0, omit_isolated = FALSE),
                 'fcm is too large for a network plot')
})

test_that("test textplot_network font-selection", {
    skip_on_os("linux")
    txt <- "A D A C E A D F E B A C E D"
    testfcm <- fcm(txt, context = "window", window = 3, tri = FALSE)
    testdfm <- dfm(txt)
    expect_silent(textplot_network(testfcm, offset = 0.1, 
                                   vertex_labelfont = "serif"))
    expect_silent(textplot_network(testdfm, offset = 0.1, 
                                   vertex_labelfont = "sans"))
    expect_error(textplot_network(testfcm, min_freq = 0.1, 
                                  vertex_labelfont = "not_a_real_font"),
                 "not_a_real_font is not found on your system")
})

test_that("raises error when dfm is empty (#1419)", {
    
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(textplot_network(mx),
                 quanteda:::message_error("dfm_empty"))
    expect_error(textplot_network(fcm(mx)),
                 quanteda:::message_error("fcm_empty"))
    expect_error(textplot_wordcloud(mx),
                 quanteda:::message_error("dfm_empty"))
    
})

test_that("remove_edges is working", {
    
    mt <- fcm(c("a a b", "a b", "c b"))
    expect_identical(colnames(quanteda:::remove_edges(mt, 1, TRUE)), 
                     c("a", "b", "c"))
    expect_identical(colnames(quanteda:::remove_edges(mt, 2, TRUE)), 
                     c("a", "b"))
    expect_identical(Matrix::diag(quanteda:::remove_edges(mt, 1, FALSE)),
                     c(0, 0, 0))
    
})

# test_that("error when fcm is ordered", {
#     
#     mt <- fcm(c("a a b", "a b", "c b"), ordered = FALSE)
#     expect_silent(as.network(mt))
#     mt2 <- fcm(c("a a b", "a b", "c b"), ordered = TRUE)
#     expect_error(as.network(mt2),
#                  "Cannot plot ordered fcm")
# })


