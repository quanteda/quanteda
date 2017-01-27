context('test tokens_compound.R')

test_that("tokens_compound join tokens correctly", {
      
    txt <- c("a b c d e f g", "A B C D E F G", "A b C d E f G", 
             "aaa bbb ccc ddd eee fff ggg", "a_b b_c c_d d_e e_f f_g") 
    toks <- tokens(txt)
    seqs <- tokens(c("a b", "C D", "aa* bb*", "eEE FFf", "d_e e_f"), 
                   hash = FALSE, what = "fastestword")
    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = TRUE)),
        list(c("a_b", "c_d", "e", "f", "g"),
             c("A_B", "C_D", "E", "F", "G"),
             c("A_b", "C_d", "E", "f", "G"),
             c("aaa_bbb", "ccc", "ddd", "eee_fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )
    
    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = FALSE)),
        list(c("a_b", "c", "d", "e", "f", "g"),
             c("A", "B", "C_D", "E", "F", "G"),
             c("A", "b", "C", "d", "E", "f", "G"),
             c("aaa_bbb", "ccc", "ddd", "eee", "fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )
    
    seqs_fixed <- tokens(c("a b", "C D", "aa bb", "eEE FFf", "d_e e_f"), 
                         hash = FALSE, what = "fastestword")
    expect_equivalent(
        as.list(tokens_compound(toks, seqs_fixed, valuetype = "glob", case_insensitive = TRUE)),
        list(c("a_b", "c_d", "e", "f", "g"),
             c("A_B", "C_D", "E", "F", "G"),
             c("A_b", "C_d", "E", "f", "G"),
             c("aaa", "bbb", "ccc", "ddd", "eee_fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )
    
    expect_equivalent(
        as.list(tokens_compound(toks, seqs_fixed, valuetype = "glob", case_insensitive = FALSE)),
        list(c("a_b", "c", "d", "e", "f", "g"),
             c("A", "B", "C_D", "E", "F", "G"),
             c("A", "b", "C", "d", "E", "f", "G"),
             c("aaa", "bbb", "ccc", "ddd", "eee", "fff", "ggg"),
             c("a_b", "b_c", "c_d", "d_e_e_f", "f_g"))
    )
})

test_that("tokens_compound join tokens from  longer sequences", {
    
    txt <- c("a b c d e f g", "A B C D E F G") 
    toks <- tokens(txt)
    seqs <- tokens(c("a b", "a b c d", "E F G", "F G"), 
                   hash = FALSE, what = "fastestword")
    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = TRUE)),
        list(c("a_b_c_d", "e_f_g"),
             c("A_B_C_D", "E_F_G"))
    )
    
    expect_equivalent(
        as.list(tokens_compound(toks, seqs, valuetype = "glob", case_insensitive = FALSE)),
        list(c("a_b_c_d", "e", "f", "g"),
             c("A", "B", "C", "D", "E_F_G"))
    )
    
})

test_that("tokens_compound always compounds the longer phrase first (#240)", {
    expect_equal(
        as.list(tokens_compound(tokens("The people of the United States of America."), 
                                c("United States of America", "United States"))),
        as.list(tokens_compound(tokens("The people of the United States of America."), 
                                c("United States", "United States of America")))
    )
})

test_that("tokens_compound preserved document names", {
    expect_equal(
        names(tokens_compound(tokens(c(d1 = "The people of the United States of America.",
                                       d2 = "The United States is south of Canada.")),
                              c("United States", "United States of America"))),
        c("d1", "d2")
    )
})

test_that("tokens_compound works with padded tokens", {
    toks <- tokens(c(doc1 = 'a b c d e f g'))
    toks <- tokens_remove(toks, c('b', 'e'), padding = TRUE)
    toks <- tokens_compound(toks, "c d")
    expect_equal(sort(attr(toks, "types")),
                 sort(c("a", "c_d", "f", "g")))
})

test_that("tokens_compound detect overlapped sequences", {
    
    txts <- 'we like high quality sound'
    seqs <- c('high quality', 'quality sound', "high quality sound")
    expect_equivalent(as.list(tokens_compound(tokens(txts), seqs, overlap = TRUE)),
                      list(c("we", "like", "high_quality_sound", "high_quality", "quality_sound")))
    expect_equivalent(as.list(tokens_compound(tokenize(txts), seqs, overlap = TRUE)),
                      list(c("we", "like", "high_quality_sound", "high_quality", "quality_sound")))
    
})


test_that("tokens_compound detect overlapped sequences", {
    
    toks <- tokenize("A B C D")
    expect_equivalent(as.list(tokens_compound(toks, c("A B", "B C"), overlap = TRUE)),
                      list(c("we", "like", "high_quality_sound", "high_quality", "quality_sound")))
    expect_equivalent(as.list(tokens_compound(tokenize(txts), seqs, overlap = TRUE)),
                      list(c("we", "like", "high_quality_sound", "high_quality", "quality_sound")))
    
})
