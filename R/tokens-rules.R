# https://unicode-org.github.io/icu/userguide/boundaryanalysis/break-rules.html

#rule_word <- readLines("rules/word.txt")
#rule_sent <- readLines("rules/sent.txt")

rules <- list()
rules[["word"]] <- readLines("rules/word.txt")

## Hyphen rule
# hyphen_rule_lax <- 
#     r"---(
# $Hyphen = [\p{Pd}];
# ($ALetterPlus | $Hebrew_Letter | $Numeric)  $ExFm* $Hyphen $ExFm* ($ALetterPlus | $Hebrew_Letter | $Numeric)? {200};
#     )---"

## Hyphen rule
rules[["hyphen"]] <- 
r"---(
$Hyphen = [\p{Pd}];
($ALetterPlus | $Hebrew_Letter | $Numeric)  $ExFm* $Hyphen $ExFm* ($ALetterPlus | $Hebrew_Letter | $Numeric) {200};
)---"

## URL and email rule
rules[["url"]] <- 
r"---(
^(([h][t][t][p][s]?\:\/\/([w][w][w]\.)?)|([w][w][w]\.))[-a-zA-Z0-9@:%_\+\.~#=]+(\/[-a-zA-Z0-9@\:%_\+\.~#?\&=]+)*[\/]?;
)---"

rules[["email"]] <- 
r"---(
[A-Za-z0-9_]+\@[A-Za-z][A-Za-z0-9_]+\.[a-z]+;
)---"


### Protect variant selector & whitespace with diacritical marks
# variantSelector_diacritical <- 
# r"-(
# $Variant = [\uFE00-\uFE0F];
# $Diacritical = [\p{whitespace}][\u0300-\u036F];
# 
# # Rules
# ($ALetterPlus | $Hebrew_Letter) $Variant ($ALetterPlus | $Hebrew_Letter);
# ($ALetterPlus | $Hebrew_Letter) $Diacritical ($ALetterPlus | $Hebrew_Letter);
# )-"

# Elision definition and rule
rules[["elision"]] <- 
r"-(
$Elision = ([lLmMtTnNsSjJdDcC]|([jJ][u][s]|[qQ][u][o][i]|[lL][o][r][s]|[pP][u][i][s])?[qQ][u])[\u0027\u2019];
# Disable chaining so it only matches beginning of word.
^$Elision / $ALetterPlus;
)-"

