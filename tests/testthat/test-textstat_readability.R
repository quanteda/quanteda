context("textstat_readability")

test_that("readability works: basic", {
    txt <- "This was adjusted by a prolongation of the period of reimbursement in nature of a new loan 
            at an interest of 5% for the term of ten years, and the expenses of this operation were a commission of 3%.  
            The first installment of the loan of $2,000,000 from the Bank of the United States has been paid, as was directed by law.  
            For the second it is necessary that provision be made.  
            No pecuniary consideration is more urgent than the regular redemption and discharge of the public debt."
    expect_true(!is.na(textstat_readability(txt, "Flesch")$Flesch))
})

test_that("readability count is ok", {
    expect_equivalent(textstat_readability("The cat in the hat ate breakfast.", "Flesch")$Flesch, 103,
                      tolerance = 0.01)
    expect_equivalent(textstat_readability("The cat in the hat ate breakfast.", "FOG")$FOG, 2.8)
})

test_that("readability works with sentence length filtering", {
    txt <- c("PAGE 1. This is a single sentence.  Short sentence. Three word sentence.",
             "PAGE 2. Very short! Shorter.",
             "Very long sentence, with multiple parts, separated by commas.  PAGE 3.")
    rdb <- textstat_readability(txt, measure = "all")
    expect_equal(rdb$meanSentenceLength, c(3, 1.67, 5.50), tolerance = 0.01)
    
    rdb2 <- textstat_readability(txt, measure = "all", min_sentence_length = 3)
    expect_equal(rdb2$meanSentenceLength, c(4, NA, 9))
})

# test_that("readability works as koRpus", {
#     skip("korPus update broke this test and made koRpus otherwise unsuable")
#     skip_if_not_installed("koRpus")
#     #q_rdb <- textstat_readability("The cat in the hat ate breakfast.")
#     #fileName <- "sample_text2.txt"  
#     #for this a bit longer file, the results differed a bit because the tokenizations are 
#     #inconsistent between two packages. 
#     fileName <- "sample_text.txt"
#     q_rdb <- textstat_readability(readChar(fileName, file.info(fileName)$size))
#     
#     # readability analysis from package koRpus
#     install.koRpus.lang("en")
#     k_toks <- koRpus::tokenize(fileName, lang = "en")
#     wordlist_DC <- data_char_wordlists$dalechall
#     k_rdb <- suppressWarnings(koRpus::readability(k_toks, 
#                                  word.lists = list(Bormuth = wordlist_DC, 
#                                                    Dale.Chall = wordlist_DC, 
#                                                    Harris.Jacobson = wordlist_DC)))
#     
#     expect_equal(round(q_rdb$ARI, 2), round(k_rdb@ARI$grade, 2))
#     expect_equal(round(q_rdb$Coleman.Liau.grade, 2), round(k_rdb@Coleman.Liau$grade, 2))
#     expect_equal(round(q_rdb$Flesch, 2), round(k_rdb@Flesch$RE, 2))
#     expect_equal(round(q_rdb$SMOG, 2), round(k_rdb@SMOG$grade, 2))
# })

test_that("Test Dale-Chall readability", {
    # from Dale, Edgar, and Jeanne S Chall. 1948. “A Formula for Predicting
    # Readability: Instructions.” Educational Research Bulletin 27(2): 37–54.

    dc1 <- "A happy, useful life - that's what you want for your baby, isn't it? And because
a healthy mind and body are so necessary to happiness and long life, you must
do all you can to get your baby off to a good start. There is much you can do
while he is still a baby to lay the foundation for good health and good health
habits. Many things affect your baby's health. One was the state of your own
health during pregnancy, and the special care your doctor gave you before the
baby was born. Other things important to your child's health are food,
clothes, baths, sleep, and habit training. A baby needs a clean, happy place
to live, and he must be kept from having any sickness that can be prevented."
    unfamiliar_words <- tokens_remove(tokens(dc1, remove_punct = TRUE), 
                                       pattern = char_tolower(data_char_wordlists$dalechall),
                                       case_insensitive = TRUE) %>%
        as.character()
    expect_identical(
        unique(unfamiliar_words),
        c("necessary", "foundation", "affect", "pregnancy", "special", "prevented")
    )
    expect_equivalent(ntoken(dc1, remove_punct = TRUE), 132)
    expect_equal(textstat_readability(dc1, "Dale.Chall.old")$Dale.Chall.old + 3.6365, 5.3684, tolerance = .1)
    
    dc2 <- "Diphtheria used to kill many babies. Today no child need die of
diphtheria.  It is one of the diseases for which we have very good treatment
and almost sure prevention. But your baby will not be safe from this disease
unless he has been protected by immunization. The way to protect your baby is
simple. Physicians usually give injections of three doses of toxoid, three to
four weeks apart, generally beginning when a baby is about six months old.
Your doctor will tell you that your baby should have this protection before
his first birthday. Six months after the last injection of toxoid, the
physician may test your baby to see if another dose of toxoid is necessary.
Before the child enters school an extra shot of toxoid is often given."
    unfamiliar_words <- tokens_remove(tokens(dc2, remove_punct = TRUE), 
                                       pattern = char_tolower(data_char_wordlists$dalechall),
                                       case_insensitive = TRUE) %>%
        as.character()
    expect_identical(
        unique(char_tolower(unfamiliar_words)),
        c("diphtheria", "diseases", "treatment", "prevention", "disease", 
          "immunization", "physicians", "usually", "injections", "doses", 
          "toxoid", "protection", "injection", "physician", "dose", "necessary")
    )
    expect_identical(length(unfamiliar_words), 20L)
    expect_equivalent(ntoken(dc2, remove_punct = TRUE), 131)
    expect_equivalent(nsentence(dc2), 9)
    expect_equal(textstat_readability(dc2, "Dale.Chall.old")$Dale.Chall.old, 6.7490, tolerance = .02)
    
    dc3 <- "The germs that cause tuberculosis can enter the baby's body through
his mouth or be breathed in through his nose. These germs come to him on spray
or moisture which the person with active tuberculosis breathes or coughs out.
Germ-filled spray from the mouth or nose may light on the baby's food, his
dishes, his toys. The baby's hands may carry germs from soiled objects to his
mouth. Kissing is one way of spreading TB as well as other germs. Tuberculosis
of the bones or joints or of certain organs of the body besides the lungs can
come to the bottle-fed baby in milk which has not been pasteurized or boiled."
    unfamiliar_words <- tokens_remove(tokens(dc3, remove_punct = TRUE), 
                                       pattern = char_tolower(data_char_wordlists$dalechall),
                                       case_insensitive = TRUE) %>%
        as.character()
    expect_identical(
        unique(char_tolower(unfamiliar_words)),
        c("germs", "tuberculosis", "spray", "moisture", "active", "germ-filled",
          "objects", "tb", "joints", "lungs", "bottle-fed", "pasteurized")
    )
    expect_equal(length(unfamiliar_words), 17, tolerance = 1)
    expect_equivalent(ntoken(dc3, remove_punct = TRUE), 111)
    expect_equivalent(nsentence(dc3), 6)
    expect_equal(textstat_readability(dc3, "Dale.Chall.old")$Dale.Chall.old, 6.9474, tolerance = .01)
})

test_that("textstat_readability with intermediate = TRUE works", {
    rs1a <- textstat_readability(data_char_sampletext, measure = "Flesch.Kincaid", intermediate = TRUE)
    rs1b <- textstat_readability(data_char_sampletext, measure = "Flesch.Kincaid", intermediate = FALSE)
    rs2 <- textstat_readability(data_char_sampletext, measure = c("Dale.Chall.old", "Flesch"), intermediate = TRUE)
    
    expect_true(
        all(c("Flesch.Kincaid", "W", "St", "C", "Sy", "W3Sy", "W2Sy", "W_1Sy", "W6C", "W7C", "Wlt3Sy") %in% names(rs1a))
    )
    expect_true(
        !any(c("W", "St", "C", "Sy", "W3Sy", "W2Sy", "W_1Sy", "W6C", "W7C", "Wlt3Sy") %in% names(rs1b))
    )
    expect_true(
        all(c("Dale.Chall.old", "Flesch", "W", "St", "C", "Sy", "W3Sy", "W2Sy", "W_1Sy", "W6C", "W7C", "Wlt3Sy", "W_wl.Dale.Chall") %in% names(rs2))
    )
    
})

test_that("textstat_readability works for renamed Bormuth.MC and Coleman.Liau.ECP", {
    expect_identical(textstat_readability(data_char_sampletext, measure = 'Bormuth'),
                      textstat_readability(data_char_sampletext, measure = 'Bormuth.MC'))
    expect_identical(textstat_readability(data_char_sampletext, measure = 'Coleman.Liau'),
                      textstat_readability(data_char_sampletext, measure = 'Coleman.Liau.ECP'))
})

test_that("textstat_readability raises error for non-included measures",{
    expect_error(textstat_readability(data_char_sampletext, measure = "Gibberish"),
                 "Invalid measure(s): Gibberish", fixed = TRUE)
})

test_that("textstat_readability computes all measures (#1701)",{
    expect_true(
        all(!is.na(textstat_readability(data_char_sampletext, measure = "all")))
    )
})

test_that("textstat_readability has a default measure (#1715)",{
    expect_identical(
        names(textstat_readability(data_char_sampletext)),
        c("document", "Flesch")
    )
})

test_that("man/textstat_readability returns NA for empty documents", {
    txt <- c(d1 = "The cat in the hat at green ham and eggs.", 
             d2 = "", 
             d3 = "Once upon a time.")
    corp <- corpus(txt)
    
    expect_equivalent(
        textstat_readability(txt, "Flesch"),
        data.frame(document = paste0("d", 1:3), 
                   Flesch = c(112.085, NA, 97.025),
                   row.names = NULL, stringsAsFactors = FALSE)
    )
    expect_equivalent(
        textstat_readability(txt, "Flesch", min_sentence_length = 5),
        data.frame(document = paste0("d", 1:3), 
                   Flesch = c(112.085, NA, NA),
                   row.names = NULL, stringsAsFactors = FALSE)
    )
    
    allstat <- textstat_readability(txt, "all")
    expect_true(all(is.na(allstat["d2", -1])))
})
