context("test corpus")

test_that("test show.corpus", {
    expect_output(
        print(corpus(c("The"))),
        "Corpus consisting of 1 document."
    )
    expect_output(
        print(corpus(c("The", "quick", "brown", "fox"))),
        "Corpus consisting of 4 documents."
    )
    expect_output(
        print(corpus(c("The", "quick", "brown", "fox"),
                     docvars = data.frame(list(test = 1:4)))),
        "Corpus consisting of 4 documents and 1 docvar."
    )
    expect_output(
        print(corpus(c("The", "quick", "brown", "fox"),
                    docvars = data.frame(list(test = 1:4)))),
        "Corpus consisting of 4 documents and 1 docvar."
    )
    expect_output(
        print(corpus(c("The", "quick", "brown", "fox"),
                    docvars = data.frame(list(test = 1:4, test2 = 1:4)))),
        "Corpus consisting of 4 documents and 2 docvars."
    )
})


test_that("test corpus constructors works for kwic", {
    kw <- kwic(data_char_sampletext, "econom*")

    # split_context = TRUE, extract_keyword = TRUE
    corp <- corpus(kw, split_context = TRUE, extract_keyword = TRUE)
    expect_is(corp, "corpus")
    expect_equal(names(docvars(corp)),
                 c("from", "to", "keyword", "context"))

    # split_context = FALSE, extract_keyword = TRUE
    expect_identical(
        docnames(corpus(kw, split_context = FALSE, extract_keyword = TRUE)),
        paste0("text1.L", as.character(kw[["from"]]))
    )
    # split_context = FALSE, extract_keyword = FALSE
    expect_identical(
        docnames(corpus(kw, split_context = FALSE, extract_keyword = FALSE)),
        paste0("text1.L", as.character(kw[["from"]]))
    )
    # split_context = TRUE, extract_keyword = FALSE
    expect_identical(
        docnames(corpus(kw, split_context = TRUE, extract_keyword = FALSE)),
        c(paste0("text1.", seq_len(nrow(kw)), ".pre"),
          paste0("text1.", seq_len(nrow(kw)), ".post"))
    )

    # test text handling for punctuation - there should be no space before the ?
    corp <- tokens(data_char_sampletext, what = "word", remove_separators = FALSE) %>%
      kwic("econom*", window = 10, separator = "") %>%
      corpus(split_context = FALSE, extract_keyword = FALSE)
    expect_identical(
        texts(corp)[2],
        c("text1.L390" = "it is decimating the domestic economy? As we are tired ")
    )

    # ; and !
    txt <- c("This is; a test!")
    expect_equivalent(
        suppressWarnings(tokens(txt, what = "word", remove_separators = FALSE) %>%
          kwic("a", window = 10, separator = "") %>%
          corpus(remove_separators = FALSE, split_context = FALSE) %>%
          texts()),
        txt
    )

    # quotes
    txt <- "This 'is' only a test!"
    expect_equivalent(
      suppressWarnings(tokens(txt, what = "word", remove_separators = FALSE) %>%
                         kwic("a", window = 10, separator = "") %>%
                         corpus(remove_separators = FALSE, split_context = FALSE) %>%
                         texts()),
      txt
    )
    txt <- "This \"is\" only a test!"
    expect_equivalent(
      suppressWarnings(tokens(txt, what = "word", remove_separators = FALSE) %>%
                         kwic("a", window = 10, separator = "") %>%
                         corpus(remove_separators = FALSE, split_context = FALSE) %>%
                         texts()),
      txt
    )
    txt <- 'This "is" only (a) test!'
    expect_equivalent(
      suppressWarnings(tokens(txt, what = "word", remove_separators = FALSE) %>%
                         kwic("a", window = 10, separator = "") %>%
                         corpus(remove_separators = FALSE, split_context = FALSE) %>%
                         texts()),
      txt
    )
    txt <- "This is only (a) test!"
    expect_equivalent(
      suppressWarnings(tokens(txt, what = "word", remove_separators = FALSE) %>%
                         kwic("a", window = 10, separator = "") %>%
                         corpus(remove_separators = FALSE, split_context = FALSE) %>%
                         texts()),
      txt
    )
    corp <- corpus(kw, split_context = TRUE, extract_keyword = FALSE)
})

test_that("test corpus constructors works for character", {

    expect_that(corpus(data_char_ukimmig2010), is_a("corpus"))

})

test_that("test corpus constructors works for data.frame", {

    df <- data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
                     some_ints = 1L:6L,
                     some_text = paste0("This is text number ", 1:6, "."),
                     some_logical = rep(c(TRUE, FALSE), 3),
                     stringsAsFactors = FALSE,
                     row.names = paste0("fromDf_", 1:6))
    corp <- corpus(df, docid_field = "row.names", text_field = "some_text")
    expect_equal(docnames(corp),
                 paste("fromDf", 1:6, sep = "_"))
    expect_error(corpus(df, text_field = "some_ints"),
                 "text_field must refer to a character mode column")
    expect_error(corpus(df, text_field = c(1, 3)),
                 "text_field must refer to a single column")
    expect_error(corpus(df, text_field = c("some_text", "letter_factor")),
                 "text_field must refer to a single column")
    expect_error(corpus(df, text_field = 0),
                 "text_field column not found or invalid")
    expect_error(corpus(df, text_field = -1),
                 "text_field column not found or invalid")
    expect_error(corpus(df, text_field = "nothing"),
                 "text_field column not found or invalid")

    expect_error(corpus(df, text_field = "some_text", docid_field = c(1, 3)),
                 "docid_field must refer to a single column")
    expect_error(corpus(df, text_field = "some_text", docid_field = c("some_text", "letter_factor")),
                 "docid_field must refer to a single column")
    expect_error(corpus(df, text_field = "some_text", docid_field = 0),
                 "docid_field column not found or invalid")
    expect_error(corpus(df, text_field = "some_text", docid_field = -1),
                 "docid_field column not found or invalid")
    expect_error(corpus(df, text_field = "some_text", docid_field = "notfound"),
                 "docid_field column not found or invalid")
})

test_that("test corpus constructor works for tm objects", {
    skip_if_not_installed("tm")
    require(tm)

    # VCorpus
    data(crude, package = "tm")    # load in a tm example VCorpus
    corp1 <- corpus(crude)
    expect_equal(substring(texts(corp1)[1], 1, 21),
                 c("reut-00001.xml"  = "Diamond Shamrock Corp"))

    data(acq, package = "tm")
    corp2 <- corpus(acq)
    expect_equal(dim(docvars(corp2)), c(50, 15))

    # SimpleCorpus
    txt <- system.file("texts", "txt", package = "tm")
    scorp <- SimpleCorpus(DirSource(txt, encoding = "UTF-8"),
                                control = list(language = "lat"))
    corp3 <- corpus(scorp)
    expect_equal(content(scorp), texts(corp3))

    # any other type
    scorp2 <- scorp
    class(scorp2)[1] <- "OtherCorpus"
    expect_error(
        corpus(scorp2),
        "Cannot construct a corpus from this tm OtherCorpus object"
    )
    detach("package:tm", unload = FALSE, force = TRUE)
    detach("package:NLP", unload = FALSE, force = TRUE)
})

test_that("test corpus constructor works for VCorpus with one document (#445)", {
    skip_if_not_installed("tm")
    require(tm)
    vcorp <- VCorpus(VectorSource(data_corpus_inaugural[2]))
    corp <- corpus(vcorp)
    expect_equivalent(texts(corp)[1], texts(data_corpus_inaugural)[2])
    detach("package:tm", unload = FALSE, force = TRUE)
    detach("package:NLP", unload = FALSE, force = TRUE)
})

test_that("test corpus constructor works for complex VCorpus (#849)", {
   skip("not implemented yet")
   skip_if_not_installed("tm")
    require(tm)
    load("../data/corpora/complex_Corpus.rda")
    corp <- corpus(complex_Corpus)
    expect_equal(
        head(docnames(corp), 3),
        c("41113_201309.1", "41113_201309.2", "41113_201309.3")
    )
    expect_equal(
        tail(docnames(corp), 3),
        c("41223_201309.2553", "41223_201309.2554", "41223_201309.2555")
    )
    expect_output(
        print(corp),
        "Corpus consisting of 8,230 documents and 16 docvars\\."
    )
})

test_that("corpus works for texts with duplicate filenames", {
    txt <- c(one = "Text one.", two = "text two", one = "second first text")
    corp <- corpus(txt, unique_docnames = FALSE)
    expect_equal(docnames(corp), c("one.1", "two.1", "one.2"))
    expect_error(corpus(txt, unique_docnames = TRUE), "docnames must be unique")
})

test_that("create a corpus on a corpus", {
    corp <- data_corpus_inaugural
    expect_equivalent(
        as.corpus(corp),
        corpus(corp)
    )

    corp <- as.corpus(corp)
    name <- paste0("d", seq_len(ndoc(corp)))
    corp2 <- corpus(corp, docnames = name, docvars = docvars(corp))
    docnames(corp) <- name
    expect_identical(
        docnames(corp),
        docnames(corp2)
    )
    expect_identical(
        docvars(corp),
        docvars(corp2)
    )
})

test_that("corpus constructor works with tibbles", {
    skip_if_not_installed("tibble")
    dd <- tibble::tibble(a = 1:3, text = c("Hello", "quanteda", "world"))
    expect_is(
        corpus(dd),
        "corpus"
    )
    expect_equal(
        texts(corpus(dd)),
        c(text1 = "Hello", text2 = "quanteda", text3 = "world")
    )
})

test_that("corpus works on dplyr grouped data.frames (#1232)", {
    skip_if_not_installed("dplyr")
    df_grouped <-
        data.frame(letter_factor = factor(rep(letters[1:3], each = 2)),
                   some_ints = 1L:6L,
                   text = paste0("This is text number ", 1:6, "."),
                   stringsAsFactors = FALSE,
                   row.names = paste0("fromDf_", 1:6)) %>%
        dplyr::group_by(letter_factor) %>%
        dplyr::mutate(n_group = dplyr::n())
    expect_output(
        print(corpus(df_grouped)),
        "^Corpus consisting of 6 documents and 3 docvars\\."
    )
})

test_that("c.corpus errors work as expected", {

  corp1 <- corpus(c(d1 = "This is sample document one.",
                    d2 = "Here is the second sample document."))
  corp2 <- corpus(c(d3 = "And the third document."))
  corp3 <- corpus(c(d4 = "This is sample document 4."))
  corp4 <- corpus(c(d1 = "This is sample document five!. This is a long document."))
  corp5 <- corpus_reshape(corp4)

  expect_equal(
    c(corp1),
    corp1
  )

  expect_equal(
    c(corp1, corp2),
    corp1 + corp2
  )

  expect_equal(
    c(corp1, corp2, corp3),
    corp1 + corp2 + corp3
  )

  # issue #1836
  expect_error(
    c(corp1, corp4),
    "Cannot combine corpora with duplicated document names"
  )
  #expect_error(
  #  c(corp1, corp5),
  #  "Cannot combine corpora in different units"
  #)

  corp <- c(data_corpus_inaugural[1:2],
            data_corpus_inaugural[3:5],
            data_corpus_inaugural[6:10])

  expect_equivalent(
    corp,
    data_corpus_inaugural[1:10]
  )

  expect_equal(
    docvars(corp),
    docvars(data_corpus_inaugural[1:10])
  )

})

test_that("corpus.data.frame sets docnames correctly", {
    txt <- c("Text one.", "Text two.  Sentence two.", "Third text is here.")
    dnames <- paste(LETTERS[1:3], "dn", sep = "-")
    rnames <- paste(LETTERS[1:3], "rn", sep = "-")

    df <- data.frame(other = txt,
                     row.names = NULL, stringsAsFactors = FALSE)
    dfdocid_ <- data.frame(other = txt, doc_id = dnames,
                           row.names = NULL, stringsAsFactors = FALSE)
    df_text <- data.frame(text = txt,
                          row.names = NULL, stringsAsFactors = FALSE)
    df_rownames <- data.frame(other = txt,
                              row.names = rnames, stringsAsFactors = FALSE)
    dfdocid__rownames <- data.frame(other = txt, doc_id = dnames,
                                    row.names = rnames, stringsAsFactors = FALSE)
    df_textdocid_ <- data.frame(text = txt,  doc_id = dnames,
                                row.names = NULL, stringsAsFactors = FALSE)
    df_text_rownames <- data.frame(text = txt,
                                   row.names = rnames, stringsAsFactors = FALSE)
    df_textdocid_rownames <- data.frame(text = txt, doc_id = dnames,
                                         row.names = rnames, stringsAsFactors = FALSE)

    expect_identical(
        docnames(corpus(df_textdocid_rownames)),
        c("A-dn", "B-dn", "C-dn")
    )
    expect_error(
        corpus(df_textdocid_rownames, docid_field = "notfound"),
        "docid_field column not found or invalid"
    )
    expect_identical(
        docnames(corpus(df_text_rownames)),
        c("A-rn", "B-rn", "C-rn")
    )
    expect_identical(
        docnames(corpus(df_text)),
        paste0(quanteda_options("base_docname"), seq_len(nrow(df_text)))
    )

    df2_text <- data.frame(df_text, new = c(99, 100, 101))
    expect_identical(
        docnames(corpus(df2_text, docid_field = "new")),
        c("99", "100", "101")
    )

    df3_text <- data.frame(df_text, new = c(TRUE, FALSE, TRUE))
    expect_identical(
        docnames(corpus(df3_text, docid_field = "new", unique_docnames = FALSE)),
        c("TRUE.1", "FALSE.1", "TRUE.2")
    )
})

test_that("corpus handles NA correctly (#1372, #1969)", {
    txt <- c("a b c", NA, "d e f")
    expect_true(!any(
        is.na(texts(corpus(txt)))
    ))
    expect_warning(
        corpus(txt),
        "NA is replaced by empty string"
    )
    expect_true(!any(
        is.na(texts(corpus(data.frame(text = txt, stringsAsFactors = FALSE))))
    ))
})

test_that("correctly handle data.frame with improper column names (#1388)", {
    df <- data.frame(text = LETTERS[1:5],
                     dvar1 = 1:5,
                     dvar2 = letters[22:26],
                     dvar3 = 6:10,
                     stringsAsFactors = FALSE)

    # when one column name is NA
    names(df)[3] <- NA
    expect_equal(
        corpus(df) %>% docvars() %>% names(),
        c("dvar1", "V2", "dvar3")
    )

    # when two column names are NA
    names(df)[3:4] <- NA
    expect_equal(
        corpus(df) %>% docvars() %>% names(),
        c("dvar1", "V2", "V3")
    )

    # when one column name is blank
    names(df)[3:4] <- c("dv", "")
    expect_equal(
        corpus(df) %>% docvars() %>% names(),
        c("dvar1", "dv", "V3")
    )

    # when two column names are blank
    names(df)[3:4] <- ""
    expect_equal(
        corpus(df) %>% docvars() %>% names(),
        c("dvar1", "V2", "V3")
    )
})

test_that("handle data.frame with improper column names and text and doc_id fields", {
    df <- data.frame(thetext = LETTERS[1:5],
                     docID = paste0("txt", 1:5),
                     dvar1 = 1:5,
                     dvar2 = letters[22:26],
                     dvar3 = 6:10,
                     stringsAsFactors = FALSE)

    names(df)[c(3, 5)] <- c(NA, "")
    corp <- corpus(df, text_field = "thetext", docid_field = "docID")

    expect_equal(names(docvars(corp)), c("V1", "dvar2", "V3"))
    expect_equal(docnames(corp), paste0("txt", 1:5))
    expect_equivalent(texts(corp), LETTERS[1:5])
})

test_that("handle data.frame variable renaming when one already exists", {
    df <- data.frame(thetext = LETTERS[1:5],
                     docID = paste0("txt", 1:5),
                     x = 1:5,
                     V3 = letters[22:26],
                     x = 6:10,
                     stringsAsFactors = FALSE)
    names(df)[c(3, 5)] <- c(NA, "")
    corp <- corpus(df, text_field = "thetext", docid_field = "docID")
    expect_equal(names(docvars(corp)), c("V1", "V3", "V3.1"))
})

test_that("upgrade_corpus is working", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    corp1 <- quanteda:::upgrade_corpus(data_corpus_pre2)
    expect_true(is.character(corp1))
    expect_true(all(c("docname_", "docid_", "segid_") %in% names(attr(corp1, "docvars"))))
    expect_true(all(!c("_document", "texts") %in% names(attr(corp1, "docvars"))))
    expect_true(is.factor(attr(corp1, "docvars")[["docid_"]]))

    corp2 <- quanteda:::upgrade_corpus(as.corpus(data_corpus_inaugural))
    expect_true(is.character(corp2))
    expect_true(all(c("docname_", "docid_", "segid_") %in% names(attr(corp2, "docvars"))))
    expect_true(all(!c("_document", "texts") %in% names(attr(corp2, "docvars"))))
    expect_true(is.factor(attr(corp2, "docvars")[["docid_"]]))
})

test_that("raise error when docnames or docvars are invalid", {
    expect_error(
        corpus(c("a b c", "b c d"), docnames = "onedoc"),
        quanteda:::message_error("docnames_mismatch")
    )
    expect_error(
        corpus(c("a b c", "b c d"), docvars = data.frame(docid_ = c("s1", "s2"))),
        quanteda:::message_error("docvars_invalid")
    )
})

test_that("docname uniqueness flag works", {
    expect_error(
        corpus(c("aa bb cc", "ccc dd"), docnames = c("text1", "text1")),
        "docnames must be unique"
    )
    expect_silent(
        corpus(c("aa bb cc", "ccc dd"), docnames = c("text1", "text1"), unique_docnames = FALSE)
    )
})

test_that("[.corpus out of bounds generates expected error", {
    corp1 <- corpus("one two three", docvars = data.frame(dvc1 = "A"))
    expect_error(corp1[2], "Subscript out of bounds")
})

test_that("corpus printing works", {
    corp <- data_corpus_inaugural[1:14]
    expect_silent(
        print(corp, max_ndoc = 0, max_nchar = 0, show_summary = FALSE)
    )
    expect_output(
        print(corp, max_ndoc = 0, max_nchar = 0, show_summary = TRUE),
        "Corpus consisting of 14 documents and 4 docvars.",
        fixed = TRUE
    )
    expect_output(
        print(corp, max_ndoc = 2, max_nchar = 10, show_summary = TRUE),
        paste0('Corpus consisting of 14 documents and 4 docvars.\n',
               '1789-Washington :\n',
               '"Fellow-Cit..."\n\n',
               '1793-Washington :\n',
               '"Fellow cit..."\n\n',
               '[ reached max_ndoc ... 12 more documents ]'),
        fixed = TRUE
    )
    expect_output(
        print(corp, max_ndoc = 2, max_nchar = 10, show_summary = FALSE),
        paste0('1789-Washington :\n',
               '"Fellow-Cit..."\n\n',
               '1793-Washington :\n',
               '"Fellow cit..."\n\n',
               '[ reached max_ndoc ... 12 more documents ]'),
        fixed = TRUE
    )
    expect_output(
        print(corp[1:2], max_ndoc = 2, max_nchar = 10, show_summary = FALSE),
        paste0('1789-Washington :\n',
               '"Fellow-Cit..."\n\n',
               '1793-Washington :\n',
               '"Fellow cit..."\n'),
        fixed = TRUE
    )
    expect_output(
        print(corpus("a b c d"), max_ndoc = -1, max_nchar = 2),
        paste0('Corpus consisting of 1 document.\n',
               'text1 :\n',
               '"a ..."\n'),
        fixed = TRUE
    )
    expect_output(
      print(corpus("a b c d"), max_ndoc = -1, max_nchar = 10),
      paste0('Corpus consisting of 1 document.\n',
             'text1 :\n',
             '"a b c d"\n'),
      fixed = TRUE
    )
    expect_output(
        print(corpus("a b c d"), max_ndoc = -1, max_nchar = -1),
        paste0('Corpus consisting of 1 document.\n',
               'text1 :\n',
               '"a b c d"\n'),
        fixed = TRUE
    )
})

test_that("as.corpus correctly sets metadata on pre-v2 corpus", {
    load("../data/pre_v2_objects/data_corpus_pre2.rda")
    expect_identical(
        meta(as.corpus(data_corpus_pre2), type = "user"),
        list(source = "Gerhard Peters and John T. Woolley. The American Presidency Project.",
             notes = "http://www.presidency.ucsb.edu/inaugurals.php",
             created = "Tue Jun 13 14:51:47 2017")
    )
    expect_true(
        all(c("package-version", "r-version", "system", "directory", "created") %in%
            names(meta(as.corpus(data_corpus_pre2), type = "system")))
    )
    expect_is(meta(as.corpus(data_corpus_pre2), "created", type = "system"),
              "Date"
    )

    # test when there is no created date
    data_corpus_pre2 <- unclass(data_corpus_pre2)
    data_corpus_pre2$metadata$created <- NULL
    class(data_corpus_pre2) <- c("corpus", class(data_corpus_pre2))
    meta(as.corpus(data_corpus_pre2), "created", type = "system")
    expect_identical(
        substring(as.character(meta(as.corpus(data_corpus_pre2), "created", type = "system")), 1, 10),
        substring(as.character(Sys.Date()), 1, 10)
    )
})

test_that("corpus indexing works as expected", {
    corp <- corpus(c(d1 = "one two three", d2 = "four five six", d3 = "seven eight"))

    expect_equal(corp[[1]], "one two three")
    expect_equal(as.character(corp[c(FALSE, TRUE, TRUE)]),
                 c(d2 = "four five six", d3 = "seven eight")
    )
    expect_equal(as.character(corp[c(2, 3)]),
                 c(d2 = "four five six", d3 = "seven eight")
    )
    expect_equal(as.character(corp[c("d2", "d3")]),
                 c(d2 = "four five six", d3 = "seven eight")
    )
    expect_equal(as.character(corp[c(-2, -3)]),
                 c(d1 = "one two three")
    )
    expect_error(corp[4], "Subscript out of bounds")
    expect_error(corp[1:4], "Subscript out of bounds")
    expect_error(corp["d4"], "Subscript out of bounds")
    expect_error(corp[c("d1", "d4")], "Subscript out of bounds")
})

test_that("printing a corpus works that has no documents", {
    corp <- corpus(c("one", "two", "three"), docvars = data.frame(dv = 1:3))
    expect_output(
      print(corpus_subset(corp, rep(FALSE, 3))),
      "Corpus consisting of 0 documents and 1 docvar."
    )
})
