context("dictionary construction")

test_that("dictionary constructors fail if all elements unnamed: explicit", {
    expect_error(dictionary(list(c("a", "b"), "c")),
                 "Dictionary elements must be named: a b c")
    expect_error(dictionary(list(first =  c("a", "b"), "c")),
                 "Unnamed dictionary entry: c")
})

test_that("dictionary constructors fail if all elements unnamed: implicit", {
    expect_error(dictionary(c("a", "b"), "c"),
                 "Dictionary elements must be named: a b c")
    expect_error(dictionary(first =  c("a", "b"), "c"),
                 "Unnamed dictionary entry: c")
})

test_that("dictionary constructors fail if a value is numeric", {
    expect_error(dictionary(list(first =  c("a", "b"), second = 2016)),
                 "Non-character entries found: 2016")
})

test_that("dictionary constructor works on list explicitly or implicitly", {
    expect_equal(dictionary(list(first =  c("a", "b"), second = "c")),
                 dictionary(first =  c("a", "b"), second = "c"))
})

test_that("dictionary constructor ignores non-character arguments in the implicit mode", {
    expect_equal(dictionary(first =  c("a", "b"), second = "c"),
                 dictionary(first =  c("a", "b"), second = "c", something = TRUE))
})

marydict <- dictionary("A CATEGORY" = c("more", "lamb", "little"),
                       "ANOTHER CATEGORY" = c("had", "mary"))

test_that("dictionary constructor works with wordstat format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.cat"),
                      marydict)
})

test_that("dictionary constructor works with Yoshikoder format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.ykd"),
                      marydict)
})

test_that("dictionary constructor works with YAML format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.yml"),
                      marydict)
})

test_that("dictionary constructor works with LIWC format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.dic"),
                      dictionary(list(A_CATEGORY = c("lamb", "little", "more"),
                                      ANOTHER_CATEGORY = c("had", "mary"))))
})

test_that("dictionary constructor works with Lexicoder format", {
    expect_equivalent(dictionary(file = "../data/dictionaries/mary.lcd"),
                      marydict)
})

test_that("read a dictionary with NA as a key", {
    testdict <- dictionary(file = "../data/dictionaries/issue-459.cat")
    expect_true("NA" %in% names(testdict$SOUTH))
})

test_that("as.yaml is working", {
    expect_equivalent(quanteda::as.yaml(marydict),
                      'A CATEGORY:\n  - more\n  - lamb\n  - little\nANOTHER CATEGORY:\n  - had\n  - mary\n')
})

test_that("dictionary works with different encoding", {
  skip_on_os("windows")
  suppressWarnings({
    
    # works without specifying encoding
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-1.cat", tolower = FALSE),
                      dictionary(list('LATIN' = c('B', 'C', 'D'), 'NON-LATIN' = c('Bh', 'Ch', 'Dh')), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/windows-1252.cat", tolower = FALSE),
                      dictionary(list('LATIN' = c('S', 'Z', 'Y'), 'NON-LATIN' = c('Š', 'Ž', 'Ÿ')), tolower = FALSE))
    
    # works if encoding is specified
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-2.cat", encoding = 'iso-8859-2', tolower = FALSE),
                      dictionary(list('LATIN' = c('C', 'D', 'E'), 'NON-LATIN' = c('Č', 'Ď', 'Ě')), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-14.cat", encoding = 'iso-8859-14', tolower = FALSE),
                      dictionary(list('LATIN' = c('B', 'C', 'D'), 'NON-LATIN' = c('Ḃ', 'Ċ', 'Ḋ')), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/shift-jis.cat", encoding = 'shift-jis', tolower = FALSE),
                      dictionary(list('LATIN' = c('A', 'I', 'U'), 'NON-LATIN' = c('あ', 'い', 'う')), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/euc-jp.cat", encoding = 'euc-jp', tolower = FALSE),
                      dictionary(list('LATIN' = c('A', 'I', 'U'), 'NON-LATIN' = c('あ', 'い', 'う')), tolower = FALSE))
  })
})

test_that("tolower is working", {
    list <- list(KEY1 = list(SUBKEY1 = c("A", "B"),
                             SUBKEY2 = c("C", "D")),
                 KEY2 = list(SUBKEY3 = c("E", "F"),
                             SUBKEY4 = c("G", "F", "I")),
                 KEY3 = list(SUBKEY5 = list(SUBKEY7 = c("J", "K")),
                             SUBKEY6 = list(SUBKEY8 = c("L"))))
    dict <- dictionary(list, tolower = FALSE)
    dict_lower <- dictionary(list, tolower = TRUE)
    
    expect_equal(names(unlist(list)), names(unlist(dict)))
    expect_equal(names(unlist(dict_lower)), names(unlist(dict)))
    expect_equal(unlist(list, use.names = FALSE), 
                 unlist(dict, use.names = FALSE))
    expect_equal(stringi::stri_trans_tolower(unlist(list, use.names = FALSE)), 
                 unlist(dict_lower, use.names = FALSE))
})

test_that("indexing for dictionary objects works", {
    testdict <- dictionary(file = "../data/dictionaries/laver-garry.cat")
    expect_true(is.dictionary(testdict[1:2]))
    expect_equal(names(testdict[1]), 'CULTURE')
    expect_equal(names(testdict[[1]][1]), 'CULTURE-HIGH')
    expect_equal(names(testdict[2]), 'ECONOMY')
    expect_equal(names(testdict[[2]][1]), '+STATE+')
    
    expect_output(
        print(testdict),
        "Dictionary object with 9 primary key entries and 2 nested levels"
    )
    expect_output(
        print(testdict[1]),
        "Dictionary object with 1 primary key entry and 2 nested levels"
    )
})

test_that("indexing for dictionary keys works", {
    dict <- dictionary(one = c("a", "b"), two = c("c", "d"))
    expect_true(is.dictionary(dict[1]))
    expect_equal(
        dict[1],
        dictionary(one = c("a", "b"))
    )

    expect_output(
        print(dict),
        "Dictionary object with 2 key entries\\."
    )
    expect_output(
        print(dict[1]),
        "Dictionary object with 1 key entry\\."
    )
    
    testdict <- dictionary(file = "../data/dictionaries/issue-459.cat")
    expect_identical(
        testdict[["SOUTH"]],
        testdict$SOUTH
    )
    
    
})


test_that("dictionary_depth works correctly", {
    dict1 <- dictionary(one = c("a", "b"), two = c("c", "d"))
    expect_equal(quanteda:::dictionary_depth(dict1), 1)
    
    dict2 <- dictionary(one = c("a", "b"), 
                        two = list(sub1 = c("c", "d"),
                                   sub2 = c("e", "f")))
    expect_equal(quanteda:::dictionary_depth(dict2), 2)
    
    expect_output(
        print(dict2),
        "Dictionary object with 2 primary key entries and 2 nested levels\\."
    )
})

test_that("as.list is working", {
    
    dict <- dictionary(one = c("a", "b"), two = c("c", "d"))
    expect_equal(
        as.list(dict),
        list(one = c("a", "b"), two = c("c", "d"))
    )
    expect_equal(
        as.list(dict)[[1]],
        c("a", "b")
    )
})

test_that("error if empty concatenator is given", {
    
    expect_error(dictionary(one = c("a", "b"), two = c("c", "d"), concatenator = ''),
                 'Concatenator cannot be null or an empty string')
    
    # expect_error(dictionary(one = c("a", "b"), two = c("c", "d"), concatenator = NULL),
    #              'Concatenator cannot be null or an empty string')
})

test_that("dictionary woks with the Yoshicoder format", {
    testdict <- dictionary(file = "../data/dictionaries/laver-garry.ykd")
    expect_equal(names(testdict), 'Laver and Garry') 
    expect_equal(names(testdict[['Laver and Garry']]), 
                 c("State in Economy", "Institutions", "Values", "Law and Order", "Environment", 
                   "Culture", "Groups", "Rural", "Urban"))
    
})    


test_that("dictionary constructor works with LIWC format w/doubled terms", {
    expect_equivalent(
        dictionary(file = "../data/dictionaries/mary_doubleterm.dic"),
        dictionary(list(A_CATEGORY = c("lamb", "little", "more"),
                        ANOTHER_CATEGORY = c("had", "little", "mary")))
    )
})

test_that("dictionary constructor works with LIWC format zero padding", {
    expect_equivalent(
        dictionary(file = "../data/dictionaries/mary_zeropadding.dic"),
        dictionary(list(A_CATEGORY = c("lamb", "little", "more"),
                        ANOTHER_CATEGORY = c("had", "little", "mary")))
    )
})

test_that("dictionary constructor reports mssing cateogries in LIWC format", {
    expect_message(
        dictionary(file = "../data/dictionaries/mary_missingcat.dic"),
        "note: ignoring undefined categories:" 
    )
})

test_that("dictionary constructor works with LIWC format w/multiple tabs, spaces, etc", {
    expect_equivalent(
        dictionary(file = "../data/dictionaries/mary_multipletabs.dic"),
        dictionary(list(A_CATEGORY = c("lamb", "little", "more"),
                        ANOTHER_CATEGORY = c("had", "little", "mary")))
    )
})

test_that("dictionary constructor works with LIWC format w/extra codes", {
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: 1 term ignored because contains unsupported <of> tag"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: ignoring parenthetical expressions in lines:"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty keys: friend, humans, insight, cause, discrep, filler"
    )
    dict <- dictionary(file = "../data/dictionaries/liwc_extracodes.dic")
    expect_equal(
        length(dict), 
        10
    )
    expect_true(setequal(
        names(dict), 
        c("verb", "past", "whatever", "family", "affect", "posemo", "cogmech", "tentat", "whatever2", "time")
    ))
    
    dict1 <- quanteda:::read_dict_liwc("../data/dictionaries/liwc_extracodes.dic")
    dict2 <- quanteda:::list2dictionary(quanteda:::read_dict_liwc_old("../data/dictionaries/liwc_extracodes.dic"))
    expect_equal(dict1[order(names(dict1))], dict2[order(names(dict2))])
})


