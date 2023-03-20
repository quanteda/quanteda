# rules for words
word <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt") 
writeLines(word, "rules/word.txt")

data_breakrules_word <- list(base = paste0(readLines("rules/word.txt"), collapse = "\n"))
data_breakrules_word <- c(data_breakrules_word, yaml::read_yaml("rules/cusotm.yml"))
save("data_breakrules_word", file = "data/data_breakrules_word.rda")

# rules for sentences
sent <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt") 
writeLines(sent, "rules/sent.txt")

data_breakrules_sentence <- list(base = paste0(readLines("rules/sent.txt"), collapse = "\n"))
save("data_breakrules_sentence", file = "data/data_breakrules_sentence.rda")
