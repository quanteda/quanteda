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
              
    testkwic <- kwic(inaugCorpus, 'american')

    expect_that(
        names(attr(testkwic, 'ntoken')),
        equals(names(texts(inaugCorpus)))
    )
})
    
test_that("test kwic general", {

    testkwic <- kwic(paste(LETTERS, collapse=' '), 'D')
    expect_that(
        data.frame(testkwic),
        equals(data.frame(
            docname=as.factor('text1'),
            position=4,
            contextPre='A B C',
            keyword='D',
            contextPost='E F G H I',
            stringsAsFactors=F
        ))
    )
})


test_that("test kwic on first token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'A')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname=as.factor('text1'),
                position=1,
                contextPre='',
                keyword='A',
                contextPost='B C D E F',
                stringsAsFactors=F
            ))
        )

})


test_that("test kwic on last token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'Z')
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname=as.factor('text1'),
                position=26,
                contextPre='U V W X Y',
                keyword='Z',
                contextPost='',
                stringsAsFactors=F
            ))
        )

})


test_that("test kwic on two tokens", {

    testkwic <- kwic(paste(LETTERS, collapse=' '), c('D', 'E'))
    expect_that(
            data.frame(testkwic),
            equals(data.frame(
                docname=as.factor('text1'),
                position='4:5',
                contextPre='A B C',
                keyword='D E',
                contextPost='F G H I J',
                stringsAsFactors=F
            ))
        )

})

test_that("test kwic on non-existant token", {
    testkwic <- kwic(paste(LETTERS, collapse=' '), 'É')
    expect_true( is.na(testkwic) )
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
                docname=as.factor('text2'),
                position=1,
                contextPre='',
                keyword='A',
                contextPost='B C D E F',
                stringsAsFactors=F
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
                docname=as.factor(c('text1', 'text1')),
                position=c(1, 27),
                contextPre=c('         ', 'V W X Y Z'),
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
            docname=as.factor(c('text1', 'text1')),
            position=c(4, 6),
            contextPre=c('        what does the', 'what does the fox say'),
            keyword=c('fox', 'fox'),
            contextPost=c('say fox', '       '),
            stringsAsFactors=F
        ))
    )
})


txt <- inaugTexts["2005-Bush"]

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


