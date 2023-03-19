word <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt") 
writeLines(word, "rules/word.txt")

sent <- readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt") 
writeLines(sent, "rules/sent.txt")

data_breakrules <- list(word = paste0(readLines("rules/word.txt"), collapse = "\n"),
                        sent = paste0(readLines("rules/sent.txt"), collapse = "\n"))
data_breakrules <- c(data_breakrules, yaml::read_yaml("rules/cusotm.yml"))

save("data_breakrules", file = "data/data_breakrules.rda")
