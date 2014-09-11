####
#### PACKAGE GLOBALS
####

#
# Note: alternative would be to have (also) system settings that would govern the creation
# of new corpus objects.  
#

SETTINGS_OPTIONS <- c("stopwords",
                      "collocations",
                      "dictionary",
                      "dictionary_regex",
                      "stem",
                      "delimiter_word",
                      "delimiter_sentence",
                      "delimiter_paragraph",
                      "clean_tolower",
                      "clean_removeDigits",
                      "clean_removePunct") 
DEFAULT_DELIM_SENTENCE <- ".!?"
DEFAULT_DELIM_WORD <- " "
DEFAULT_DELIM_PARAGRAPH <- "\n\n"

save(SETTINGS_OPTIONS, DEFAULT_DELIM_SENTENCE, DEFAULT_DELIM_WORD, DEFAULT_DELIM_PARAGRAPH, file="../data/globals.RData")