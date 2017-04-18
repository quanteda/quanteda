context('test kwic.R')

test_that("test attr(kwic, 'ntoken') with un-named texts", {

    testkwic <- kwic(texts(c(
     'The quick brown fox jumped over the lazy dog',
     'The quick brown fox',
     'The quick brown dog jumped over the lazy dog',
     NA
     )), 'fox')

    testntokens <- c(9, 4, 9, 1)
    names(testntokens) <- c('text1', 'text2', 'text3', 'text4')

    expect_that(
        attr(testkwic, 'ntoken'),
        equals(testntokens)
    )
})

test_that("test attr(kwic, 'ntoken') text names", {
              
    testkwic <- kwic(data_corpus_inaugural, 'american')

    expect_that(
        names(attr(testkwic, 'ntoken')),
        equals(names(texts(data_corpus_inaugural)))
    )
})
    
test_that("test kwic general", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'D')
    
    dtf <- data.frame(
        docname = c('text1'),
        from = 4L,
        to = 4L,
        pre = 'A B C',
        keyword = 'D',
        post = 'E F G H I',
        stringsAsFactors = FALSE)
    
    expect_equal(
        data.frame(testkwic),
        dtf)
    
    #tokenizedTexts
    testkwic <- kwic(tokenize(paste(LETTERS, collapse=' ')), 'D')
    expect_equal(
        data.frame(testkwic),
        dtf) 
})


test_that("test kwic on first token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'A')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c('text1'),
            from = 1L,
            to = 1L,
            pre = '',
            keyword = 'A',
            post = 'B C D E F',
            stringsAsFactors = FALSE
        ))
    )
})


test_that("test kwic on last token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'Z')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c('text1'),
            from = 26L,
            to = 26L,
            pre = 'U V W X Y',
            keyword = 'Z',
            post = '',
            stringsAsFactors = FALSE
        ))
    )
})


test_that("test kwic on two tokens", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), c('D', 'E'))
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c('text1'),
            from = 4L,
            to = 5L,
            pre = 'A B C',
            keyword = 'D E',
            post = 'F G H I J',
            stringsAsFactors = FALSE
        ))
    )
})

test_that("test kwic on non-existant token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'É')
    expect_true(is.data.frame(testkwic) )
})

test_that("test kwic on multiple texts", {
    testcorpus <- corpus(c(
        paste(LETTERS[2:26], collapse = ' '),
        paste(LETTERS, collapse = ' ')
    ))
    testkwic <- kwic(testcorpus, 'A')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c('text2'),
            from = 1L,
            to = 1L,
            pre = '',
            keyword = 'A',
            post = 'B C D E F',
            stringsAsFactors = FALSE
        ))
    )
})

test_that("test kwic with multiple matches", {
    testcorpus <- corpus(c(
        paste(c(LETTERS, LETTERS), collapse = ' ')
    ))
    testkwic <- kwic(testcorpus, 'A')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c(c('text1', 'text1')),
            from = c(1L, 27L),
            to = c(1L, 27L),
            pre = c('', 'V W X Y Z'),
            keyword = c('A', 'A'),
            post = c('B C D E F', 'B C D E F'),
            stringsAsFactors = F
        ))
    )
})

test_that("test kwic with multiple matches, where one is the last (fixed bug)", {
    testkwic <- kwic('what does the fox say fox', 'fox')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = c(c('text1', 'text1')),
            from = c(4L, 6L),
            to = c(4L, 6L),
            pre = c('what does the', 'what does the fox say'),
            keyword = c('fox', 'fox'),
            post = c('say fox', ''),
            stringsAsFactors = F
        ))
    )
})


txt <- data_char_inaugural["2005-Bush"]

test_that("test that kwic works for glob types", {
    kwicGlob <- kwic(txt, "secur*", window = 3, valuetype = "glob", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 unique(kwicGlob$keyword))
    )
    
    kwicGlob <- kwic(txt, "secur*", window = 3, valuetype = "glob", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 unique(kwicGlob$keyword))
    )
    
})

test_that("test that kwic works for regex types", {
    kwicRegex <- kwic(txt, "^secur", window = 3, valuetype = "regex", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "secured", "securing", "Security"),
                 unique(kwicRegex$keyword))
    )
    
    kwicRegex <- kwic(txt, "^secur", window = 3, valuetype = "regex", case_insensitive = FALSE)
    expect_true(
        setequal(c("security", "secured", "securing"),
                 unique(kwicRegex$keyword))
    )
    
})

test_that("test that kwic works for fixed types", {
    kwicFixed <- kwic(inaugTexts, "security", window = 3, valuetype = "fixed", case_insensitive = TRUE)
    expect_true(
        setequal(c("security", "Security"),
                 unique(kwicFixed$keyword))
    )
    
    kwicFixed <- kwic(inaugTexts, "security", window = 3, valuetype = "fixed", case_insensitive = FALSE)
    expect_true(
        setequal(c("security"),
                 unique(kwicFixed$keyword))
    )
})

test_that("is.kwic works as expected", {
    mykwic <- kwic(data_corpus_inaugural, "provident*")
    expect_true(is.kwic(mykwic))
    expect_false(is.kwic("Not a kwic"))
})

test_that("textplot_xray works with new kwic, one token phrase", {
    data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralPost70, "american")
    expect_silent(textplot_xray(knew))
})

test_that("textplot_xray works with new kwic, two token phrase", {
    data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralPost70, "american people", new = TRUE)
    expect_silent(textplot_xray(knew))
})

test_that("textplot_xray works with new kwic, two token phrase", {
    data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralPost70, "american people", new = TRUE)
    expect_silent(textplot_xray(knew))
})

test_that("print method works as expected", {
    testkwic <- kwic('what does the fox say fox', 'fox')
    expect_output(print(testkwic), "*\\| fox \\|*")
    expect_output(print(testkwic), "\\[text1, 4\\]*")
    
    testkwic <- kwic('what does the fox say fox', 'foox')
    expect_null(print(testkwic))
})


test_that("kwic works with padding", {
    testtoks <- tokens('what does the fox say cat')
    expect_output(print(kwic(tokens_remove(testtoks, c('what', 'the'), padding = TRUE), 'fox')),
                  '\\[text1, 4\\]  does \\| fox \\| say cat')
    expect_null(print(kwic(tokens_remove(testtoks, '*', padding = TRUE), 'fox')))
    
})

test_that("as.tokens is working", {
    testkwic <- kwic('what does the fox say fox', 'fox', window = 1)
    testtoks <- as.tokens(testkwic)
    expect_equivalent(as.list(testtoks),
                      list(c("the", "fox", "say"), c("say", "fox")))
    
    testdfm <- dfm(testtoks)
    expect_equivalent(as.vector(testdfm[1,]),
                      c(1, 1, 1))
    expect_equivalent(as.vector(testdfm[2,]),
                      c(0, 1, 1))
})



