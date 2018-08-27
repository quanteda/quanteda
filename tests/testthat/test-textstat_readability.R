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
    rdb <- textstat_readability(txt)
    expect_equal(rdb$meanSentenceLength, c(3, 1.67, 5.50), tolerance = 0.01)
    
    rdb2 <- textstat_readability(txt, min_sentence_length = 3)
    expect_equal(rdb2$meanSentenceLength, c(4, 9))
})

test_that("readability works as koRpus", {
    skip_if_not_installed("koRpus")
    #q_rdb <- textstat_readability("The cat in the hat ate breakfast.")
    #fileName <- "sample_text2.txt"  
    #for this a bit longer file, the results differed a bit because the tokenizations are 
    #inconsistent between two packages. 
    fileName <- "sample_text.txt"
    q_rdb <- textstat_readability(readChar(fileName, file.info(fileName)$size))
    
    # readability analysis from package koRpus
    k_toks <- koRpus::tokenize(fileName, lang = "en")
    wordlist_DC <- data_char_wordlists$dalechall
    k_rdb <- koRpus::readability(k_toks, 
                                 word.lists = list(Bormuth = wordlist_DC, 
                                                   Dale.Chall = wordlist_DC, 
                                                   Harris.Jacobson = wordlist_DC))
    
    expect_equal(round(q_rdb$ARI, 2), round(k_rdb@ARI$grade, 2))
    expect_equal(round(q_rdb$Coleman.Liau.grade, 2), round(k_rdb@Coleman.Liau$grade, 2))
    expect_equal(round(q_rdb$Flesch, 2), round(k_rdb@Flesch$RE, 2))
    expect_equal(round(q_rdb$SMOG, 2), round(k_rdb@SMOG$grade, 2))
})

test_that("Test Dale-Chall readability", {
    # from Jeanne Chall and Edgar Dale’s Readability Revisited: 
    # The New Dale-Chall Readability Formula featured the following text samples, 
    # with the difficult words not found on their new word list underlined (pp. 135-140).
    # see http://www.impact-information.com/scales.pdf
    

    # Readability Data
    # Number of Words in Sample ........................100 
    # Number of Whole Sentences...........................8 
    # Number of Unfamiliar Words..........................3 
    # Cloze Score .......................................53 
    # Reading Level ..................................... 3
    txt <- "Once upon a time a very small witch was walking in the woods. The cold wind was blowing the dry leaves all around her. The little witch was frantically searching for a house for the winter. She could not find one. Suddenly a piece of orange paper, blown by the wind, landed at her feet. She picked it up. The little witch looked closely at the paper and then she said, “I shall make myself a little house from this piece of orange paper.”
She folded the paper in half. then she took her scissors (she always carried a pair..."
    expect_equal(textstat_readability(txt, measure = c("Dale.Chall.old"))$Dale.Chall.old, 3)
    
    toks[[1]][which(!as.character(tokens_wordstem(toks)) %in% 
                    c(char_wordstem(data_char_wordlists$dalechall), data_char_wordlists$dalechall))]
    
    # from http://www.readabilityformulas.com/dalechallformula/dale-chall-formula.php
    # (show words NOT on Dale-Chall Word List) # of words NOT found on Dale-Chall Word List : 3 
    # Percent of words NOT found on Dale-Chall Word List: : 14% 
    # 
    # Dale-Chall Formula worksheet 
    # Raw score 2.7765 [ ? ]
    # Adjusted Score: (3.6365 + 2.7765) [ ? ]
    # Final Score: 6.4 [ ? ]
    
    
})

# add UK spellings
dc_wordlist <- c(
    data_char_wordlists$dalechall,
    # and the UK spellings of words that are different
    as.tokens(list(dc = data_char_wordlists$dalechall)) %>%
        tokens_replace(data_dictionary_us2uk) %>%
        tokens_select(names(data_dictionary_us2uk)) %>%
        as.character()
)

dc <- data.frame(word = dc_wordlist,
                 stem = char_wordstem(dc_wordlist), 
                 stringsAsFactors = FALSE)
dc$diff <- dc$word != dc$stem
library(quanteda.dictionaries)




