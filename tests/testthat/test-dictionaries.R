context("dictionary construction")

test_that("dictionary constructors fail if all elements unnamed: explicit", {
    expect_error(dictionary(list(c("a", "b"), "c")),
                 "Dictionary elements must be named: a b c")
    expect_error(dictionary(list(first =  c("a", "b"), "c")),
                 "Unnamed dictionary entry: c")
})

test_that("dictionary constructors fail if all elements unnamed: implicit", {
    expect_error(dictionary(list(c("a", "b"), "c")),
                 "Dictionary elements must be named: a b c")
    expect_error(dictionary(list(first =  c("a", "b"), "c")),
                 "Unnamed dictionary entry: c")
})

test_that("dictionary constructors fail if a value is numeric", {
    expect_error(dictionary(list(first =  c("a", "b"), second = 2016)),
                 "Non-character entries found in dictionary key 'second'")
    expect_error(dictionary(list(first =  c("a", "b"), second = c("c", NA))),
                 "Non-character entries found in dictionary key 'second'")
})

test_that("dictionary constructor ignores extra arguments", {
    expect_error(
        dictionary(list(first =  c("a", "b"), second = "c"), something = TRUE),
        "unused argument \\(something = TRUE\\)"
    )
})

marydict <- dictionary(list("A CATEGORY" = c("more", "lamb", "little"),
                       "ANOTHER CATEGORY" = c("had", "mary")))

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
                      "A CATEGORY:\n  - more\n  - lamb\n  - little\nANOTHER CATEGORY:\n  - had\n  - mary\n")
})

test_that("dictionary works with different encoding", {
  skip_on_os("windows")
  suppressWarnings({

    # works without specifying encoding
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-1.cat", tolower = FALSE),
                      dictionary(list("LATIN" = c("B", "C", "D"), "NON-LATIN" = c("Bh", "Ch", "Dh")), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/windows-1252.cat", tolower = FALSE,
                                 encoding = "windows-1252"),
                      dictionary(list("LATIN" = c("S", "Z", "Y"), "NON-LATIN" = c("Š", "Ž", "Ÿ")), tolower = FALSE))

    # works if encoding is specified
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-2.cat", encoding = "iso-8859-2",
                                 tolower = FALSE),
                      dictionary(list("LATIN" = c("C", "D", "E"), "NON-LATIN" = c("Č", "Ď", "Ě")), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/iso-8859-14.cat", encoding = "iso-8859-14",
                                 tolower = FALSE),
                      dictionary(list("LATIN" = c("B", "C", "D"), "NON-LATIN" = c("Ḃ", "Ċ", "Ḋ")), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/shift-jis.cat", encoding = "shift-jis", tolower = FALSE),
                      dictionary(list("LATIN" = c("A", "I", "U"), "NON-LATIN" = c("あ", "い", "う")), tolower = FALSE))
    expect_equivalent(dictionary(file = "../data/dictionaries/euc-jp.cat", encoding = "euc-jp", tolower = FALSE),
                      dictionary(list("LATIN" = c("A", "I", "U"), "NON-LATIN" = c("あ", "い", "う")), tolower = FALSE))
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
    expect_equal(names(testdict[1]), "CULTURE")
    expect_equal(names(testdict[[1]][1]), "CULTURE-HIGH")
    expect_equal(names(testdict[2]), "ECONOMY")
    expect_equal(names(testdict[[2]][1]), "+STATE+")

    expect_output(
        print(testdict),
        "Dictionary object with 9 primary key entries and 2 nested levels"
    )
    expect_output(
        print(testdict[1]),
        "Dictionary object with 1 primary key entry and 2 nested levels"
    )
})

test_that("dictionary printing works", {
    dict <- dictionary(list(one = c("a", "b"), two = c("c", "d")))
    expect_true(is.dictionary(dict[1]))

    expect_output(
        print(dict),
        paste0(
            "Dictionary object with 2 key entries.\n",
            "- [one]:\n",
            "  - a, b\n",
            "- [two]:\n",
            "  - c, d"
        ),
        fixed = TRUE
    )
    expect_output(
        print(dict, max_nkey = 1),
        paste0(
            "Dictionary object with 2 key entries.\n",
            "- [one]:\n",
            "  - a, b\n",
            "[ reached max_nkey ... 1 more key ]"
        ),
        fixed = TRUE
    )
    expect_output(
        print(dict, max_nkey = -1, max_nval = -1),
        paste0(
            "Dictionary object with 2 key entries.\n",
            "- [one]:\n",
            "  - a, b\n",
            "- [two]:\n",
            "  - c, d"
        ),
        fixed = TRUE
    )
    expect_output(
        print(dict, max_nkey = 1, max_nval = 1),
        paste0(
            "Dictionary object with 2 key entries.\n",
            "- [one]:\n",
            "  - a [ ... and 1 more ]\n",
            "[ reached max_nkey ... 1 more key ]"
        ),
        fixed = TRUE
    )

    expect_output(
        print(dict, max_nkey = 1, max_nval = 1, show_summary = FALSE),
        paste0(
            "- [one]:\n",
            "  - a [ ... and 1 more ]\n",
            "[ reached max_nkey ... 1 more key ]"
        ),
        fixed = TRUE
    )
    
    # print nested leves (#1967)
    lis <- as.list(letters[1:10])
    names(lis) <- LETTERS[1:10]
    dict2 <- dictionary(list("letters" = lis))
    expect_output(
      print(dict2),
      "- [letters]:\n",
      fixed = TRUE
    )
    expect_output(
        print(dict2, max_nkey = 5),
        "[ reached max_nkey ... 5 more keys ]",
        fixed = TRUE
    )
    expect_output(
      print(dict2, max_nkey = -1),
      paste0(
        "  - [J]:\n",
        "    - j"
      ),
      fixed = TRUE
    )
    
    expect_output(
      print(dict, 0, 0),
      "Dictionary object with 2 key entries.",
      fixed = TRUE
    )
})


test_that("dictionary_depth works correctly", {
    dict1 <- dictionary(list(one = c("a", "b"), two = c("c", "d")))
    expect_equal(quanteda:::dictionary_depth(dict1), 1)

    dict2 <- dictionary(list(one = c("a", "b"),
                        two = list(sub1 = c("c", "d"),
                                   sub2 = c("e", "f"))))
    expect_equal(quanteda:::dictionary_depth(dict2), 2)

    expect_output(
        print(dict2),
        "Dictionary object with 2 primary key entries and 2 nested levels\\."
    )
})

test_that("as.list is working", {

    lis <- list(top1 = c("a", "b"), top2 = c("c", "d"),
                top3 = list(sub1 = c("e", "f"),
                            sub2 = c("f", "h")))
    dict <- dictionary(lis)
    expect_equal(
        as.list(dict),
        lis
    )
    expect_equal(
      as.list(dict, flatten = FALSE, levels = 1),
      lis
    )
    expect_equal(
        as.list(dict, flatten = TRUE),
        list(top1 = c("a", "b"),
             top2 = c("c", "d"),
             top3.sub1 = c("e", "f"),
             top3.sub2 = c("f", "h"))
    )
    expect_equal(
      as.list(dict, flatten = TRUE, levels = 1),
      list(top1 = c("a", "b"),
           top2 = c("c", "d"),
           top3 = c("e", "f", "f", "h"))
    )
    expect_equal(
      as.list(dict, flatten = TRUE, levels = 2),
      list(sub1 = c("e", "f"),
           sub2 = c("f", "h"))
    )
})

test_that("error if empty separator is given", {
    expect_error(dictionary(list(one = c("a", "b"), two = c("c", "d")), separator = ""),
                 "separator must be a non-empty character")
    expect_error(dictionary(list(one = c("a", "b"), two = c("c", "d")), separator = NULL),
                 "separator must be a non-empty character")
})

test_that("dictionary woks with the Yoshicoder format", {
    testdict <- dictionary(file = "../data/dictionaries/laver-garry.ykd")
    expect_equal(names(testdict), "Laver and Garry")
    expect_equal(names(testdict[["Laver and Garry"]]),
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
        "note: removing empty key: filler"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: discrep"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: cause"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: insight"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: humans"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: friend"
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

    # dict1 <- quanteda:::read_dict_liwc("../data/dictionaries/liwc_extracodes.dic")
    # dict2 <- quanteda:::list2dictionary(quanteda:::read_dict_liwc_old("../data/dictionaries/liwc_extracodes.dic"))
    # expect_equal(dict1[order(names(dict1))], dict2[order(names(dict2))])
})

test_that("dictionary constructor works with LIWC format w/extra codes and nesting", {
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_hierarchical.dic"),
        "note: 1 term ignored because contains unsupported <of> tag"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_hierarchical.dic"),
        "note: ignoring parenthetical expressions in lines:"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: filler"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: discrep"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: cause"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: insight"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: humans"
    )
    expect_message(
        dictionary(file = "../data/dictionaries/liwc_extracodes.dic"),
        "note: removing empty key: friend"
    )
    dict <- dictionary(file = "../data/dictionaries/liwc_hierarchical.dic")
    expect_equal(length(dict), 4)
    expect_equal(length(dict[[1]]), 1)
    expect_equal(length(dict[[2]]), 4)
    expect_equal(length(dict[[3]]), 2)
    expect_equal(length(dict[[4]]), 2)
})


test_that("dictionary works with yoshicoder, issue 819", {
    expect_equal(
        as.list(dictionary(file = "../data/dictionaries/issue-819.ykd")),
        list("Dictionary" = list("pos" = list("A" = "a word", "B" = "b word"))))
})

test_that("dictionary constructor works on a dictionary", {
    dictlist <- list(one = LETTERS[1:2], Two = letters[1:3], three = c("E f", "g"))
    dict <- dictionary(dictlist, tolower = FALSE)
    expect_identical(
        dict,
        dictionary(dict, tolower = FALSE)
    )

    dictlist2 <- list(one = LETTERS[1:2], Two = letters[1:3], three = c("E_f", "g"))
    dict2 <- dictionary(dictlist, tolower = FALSE)
    expect_equal(
        dictionary(dictlist, tolower = FALSE, separator = "_"),
        dictionary(dict2, tolower = FALSE, separator = "_")
    )
    expect_equal(
        as.list(dictionary(dict2, separator = "_", tolower = FALSE)),
        dictlist
    )
})

test_that("combine method is working with dictionary objects", {

    dict1 <- dictionary(list(A = c("aa", "aaa")), separator = "+")
    dict2 <- dictionary(list(B = c("b", "bb")), separator = "-")
    dict3 <- dictionary(list(A = c("aaaa", "aaaaa")))
    expect_equal(c(dict1, dict2),
                 dictionary(list(A = c("aa", "aaa"), B = c("b", "bb")), separator = "+"))
    expect_equal(c(dict1, dict2, dict3),
                 dictionary(list(A = c("aa", "aaa"), B = c("b", "bb"), A = c("aaaa", "aaaaa")), 
                            separator = "+"))
})

test_that("dictionary constructor clean values", {

    dict1 <- dictionary(list(A = c("aa  ", "  aaa  ")))
    dict2 <- dictionary(list(B = c("b", "bb", "bb")))

    # trim whitespaces
    expect_equal(dict1,
                 dictionary(list(A = c("aa", "aaa"))))

    # remove duplicates
    expect_equal(dict2,
                 dictionary(list(B = c("b", "bb"))))
})

test_that("dictionary merge values in duplicate keys", {

    dict <- dictionary(list(A = "a",
                            A = "aa",
                            A = "aaa",
                            B = list(BB = "bb"),
                            B = list(BB = "bbb"),
                            C = "c"))

    expect_equal(dict,
                 dictionary(list(A = c("a", "aa", "aaa"),
                                 B = list(BB = c("bb", "bbb")),
                                 C = "c")))

})

test_that("pattern2list() preserves the order of keys and values", {

  type <- stopwords()
  dict1 <- dictionary(list(th = c("tho*", "the"), wh = "wh*", ng = "not *"))
  dict2 <- dictionary(list(ng = "not *", th = c("tho*", "the"), wh = "wh*"))
  dict3 <- dictionary(list(ng = "not *", wh = "wh* is", th = c("tho*", "the")))
  dict4 <- dictionary(list(ng = "not *", th = c("tho*", "the"), wh = "wh* is"))

  ids1 <- quanteda:::pattern2list(dict1, type, "glob", FALSE)
  expect_identical(unique(names(ids1)), names(dict1))

  ids2 <- quanteda:::pattern2list(dict2, type, "glob", FALSE)
  expect_identical(unique(names(ids2)), names(dict2))

  ids3 <- quanteda:::pattern2list(dict3, type, "glob", FALSE)
  expect_identical(unique(names(ids3)), names(dict3))

  ids4 <- quanteda:::pattern2list(dict4, type, "glob", FALSE)
  expect_identical(unique(names(ids4)), names(dict4))
})


test_that("split_values() handle concatenators correctly", {

    expect_identical(
        quanteda:::split_values(list(A = "a_a", "b_b"), "_", " "),
        list(A = c("a", "a"), A = "a a", c("b", "b"), "b b")
    )
    expect_identical(
        quanteda:::split_values(list(A = "a_a", B = "b_b"), "_", " "),
        list(A = c("a", "a"), A = "a a", B = c("b", "b"), B = "b b")
    )
    expect_identical(
        quanteda:::split_values(list(A = "a_a", "A_A"), "_", "-"),
        list(A = c("a", "a"), A = "a-a", c("A", "A"), "A-A")
    )
    expect_identical(
        quanteda:::split_values(list(A = "a_a", "A_A"), " ", " "),
        list(A = "a_a", "A_A")
    )

})
