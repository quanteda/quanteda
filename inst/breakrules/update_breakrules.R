# read RBBI rules from ICU sources and update static files

# rules for words
word <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt") 
writeLines(word, "../inst/breakrules/breakrules_word.txt")

# rules for sentences
sent <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt") 
writeLines(word, "../inst/breakrules/breakrules_sentence.txt")
