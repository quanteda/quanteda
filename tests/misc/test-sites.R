sites <- read.csv("tests/data/sites.csv", row.names = 1, header = TRUE) %>%
    as.dfm()

avgp <- colMeans(sites)

# difference between s1 and s2
sqrt(sum((sites[1, ] - sites[2, ])^2 / avgp))

# difference between s1 and s3
sqrt(sum((sites[1, ] - sites[3, ])^2 / avgp))

# difference between s2 and s3
sqrt(sum((sites[2, ] - sites[3, ])^2 / avgp))


mt <- dfm(c("a a b c", "a b b d", "b d d e e", "e e e"))
mtprof <- dfm_weight(mt, "prop")
avgp <- colMeans(mtprof)

# canned methods
#
textstat_dist(mt, method = "chisquared")
##          text1    text2    text3
## text2 1.916667                  
## text3 3.708667 1.325333         
## text4 5.783333 4.866667 2.165333
as.dist(ExPosition::chi2Dist(as.matrix(mt))$D)
##          text1    text2    text3
## text2 1.916667                  
## text3 3.708667 1.325333         
## text4 5.783333 4.866667 2.165333

# based on http://www.econ.upf.edu/~michael/stanford/maeb4.pdf Eq 4.8
#
# difference between text1 and text2
sqrt(sum((mtprof[1, ] - mtprof[2, ])^2 / avgp))
## [1] 1.407518
# difference between text1 and text3
sqrt(sum((mtprof[1, ] - mtprof[3, ])^2 / avgp))
## [1] 1.945666
# difference between text1 and text4
sqrt(sum((mtprof[1, ] - mtprof[4, ])^2 / avgp))
## [1] 2.335302

