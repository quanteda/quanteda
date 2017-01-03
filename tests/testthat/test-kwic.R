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
    expect_equal(
        data.frame(testkwic),
        data.frame(
            docname = 'text1',
            position = "4:4",
            contextPre = ' A B C',
            keyword = 'D',
            contextPost = 'E F G H I',
            stringsAsFactors = FALSE)
        )
})



test_that("test kwic on first token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'A')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname = ('text1'),
                position = "1:1",
                contextPre = '',
                keyword = 'A',
                contextPost = 'B C D E F',
                stringsAsFactors = FALSE
            ))
        )

})


test_that("test kwic on last token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'Z')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname = ('text1'),
                position = "26:26",
                contextPre = 'U V W X Y',
                keyword = 'Z',
                contextPost = '',
                stringsAsFactors = FALSE
            ))
        )

})


test_that("test kwic on two tokens", {

    testkwic <- kwic(paste(LETTERS, collapse=' '), c('D', 'E'))
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname = ('text1'),
                position = '4:5',
                contextPre = ' A B C',
                keyword = 'D E',
                contextPost = 'F G H I J',
                stringsAsFactors = FALSE
            ))
        )

})

test_that("test kwic on non-existant token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'É')
    expect_true( is.data.frame(testkwic) )
})

test_that("test kwic on multiple texts", {
    testcorpus <- corpus(c(
       paste(LETTERS[2:26], collapse=' '),
       paste(LETTERS, collapse=' ')
    ))
    testkwic <- kwic(testcorpus, 'A')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname = ('text2'),
                position = "1:1",
                contextPre='',
                keyword='A',
                contextPost='B C D E F',
                stringsAsFactors=FALSE
            ))
        )
})

test_that("test kwic with multiple matches", {
    testcorpus <- corpus(c(
       paste(c(LETTERS, LETTERS), collapse=' ')
    ))

    testkwic <- kwic(testcorpus, 'A')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname = (c('text1', 'text1')),
                position = c("1:1", "27:27"),
                contextPre = c('', 'V W X Y Z'),
                keyword=c('A', 'A'),
                contextPost=c('B C D E F', 'B C D E F'),
                stringsAsFactors=F
            ))
        )

})

test_that("test kwic with multiple matches, where one is the last (fixed bug)", {
    testkwic <- kwic('what does the fox say fox', 'fox')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname = (c('text1', 'text1')),
            position = c("4:4", "6:6"),
            contextPre=c(' what does the', 'what does the fox say'),
            keyword=c('fox', 'fox'),
            contextPost=c('say fox ', ''),
            stringsAsFactors=F
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
    knew <- kwic(data_corpus_inauguralPost70, "american", new = TRUE)
    kold <- kwic(data_corpus_inauguralPost70, "american", new = FALSE)
    expect_silent(textplot_xray(kold))
    expect_silent(textplot_xray(knew))
})

test_that("textplot_xray works with new kwic, two token phrase", {
    data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralPost70, "american people", new = TRUE)
    kold <- kwic(data_corpus_inauguralPost70, "american people", new = FALSE)
    expect_silent(textplot_xray(kold))
    expect_silent(textplot_xray(knew))
})


