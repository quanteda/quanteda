word <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt") 
writeLines(word, "rules/word.txt")

sent <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt")
writeLines(sent, "rules/sent.txt")
