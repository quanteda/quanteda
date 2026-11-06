# Customizable tokenizer

Allows users to tokenize texts using customized boundary rules. See the
[ICU
website](https://unicode-org.github.io/icu/userguide/boundaryanalysis/break-rules.html)
for how to define boundary rules.

Tools for custom word and sentence breakrules, to retrieve, set, or
reset them to package defaults.

## Usage

``` r
tokenize_custom(x, rules)

breakrules_get(what = c("word", "sentence"))

breakrules_set(x, what = c("word", "sentence"))

breakrules_reset(what = c("word", "sentence"))
```

## Source

<https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt>

<https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt>

## Arguments

- x:

  character vector for texts to tokenize

- rules:

  a list of rules for rule-based boundary detection

- what:

  character; which set of rules to return, one of `"word"` or
  `"sentence"`

## Value

`tokenize_custom()` returns a list of characters containing tokens.

`breakrules_get()` returns the existing break rules as a list.

`breakrules_set()` returns nothing but reassigns the global breakrules
to `x`.

`breakrules_reset()` returns nothing but reassigns the global breakrules
to the system defaults. These rules are defined in
`system.file("breakrules/")`.

## Details

The package contains internal sets of rules for word and sentence
breaks, which are lists of rules for word and sentence boundary
detection. `base` is copied from the ICU library. Other rules are
created by the package maintainers in
`system.file("breakrules/breakrules_custom.yml")`.

This function allows modification of those rules, and applies them as a
new tokenizer.

Custom word rules:

- `base`:

  ICU's rules for detecting word/sentence boundaries

- `keep_hyphens`:

  quanteda's rule for preserving hyphens

- `keep_url`:

  quanteda's rule for preserving URLs

- `keep_email`:

  quanteda's rule for preserving emails

- `keep_tags`:

  quanteda's rule for preserving tags

- `split_elisions`:

  quanteda's rule for splitting elisions

- `split_tags`:

  quanteda's rule for splitting tags

## Examples

``` r
lis <- tokenize_custom("a well-known http://example.com", rules = breakrules_get("word"))
tokens(lis, remove_separators = TRUE)
#> Tokens consisting of 1 document.
#> text1 :
#> [1] "a"           "well-known"  "http"        ":"           "/"          
#> [6] "/"           "example.com"
#> 
breakrules_get("word")
#> $base
#> [1] "#\n# Copyright (C) 2016 and later: Unicode, Inc. and others.\n# License & terms of use: http://www.unicode.org/copyright.html\n# Copyright (C) 2002-2016, International Business Machines Corporation\n# and others. All Rights Reserved.\n#\n# file:  word.txt\n#\n# ICU Word Break Rules\n#      See Unicode Standard Annex #29.\n#      These rules are based on UAX #29 Revision 34 for Unicode Version 12.0\n#\n# Note:  Updates to word.txt will usually need to be merged into\n#        word_POSIX.txt and word_fi_sv.txt also.\n\n##############################################################################\n#\n#  Character class definitions from TR 29\n#\n##############################################################################\n\n!!chain;\n!!quoted_literals_only;\n\n\n#\n#  Character Class Definitions.\n#\n\n$Han                = [:Han:];\n\n$CR                 = [\\p{Word_Break = CR}];\n$LF                 = [\\p{Word_Break = LF}];\n$Newline            = [\\p{Word_Break = Newline}];\n$Extend             = [\\p{Word_Break = Extend}-$Han];\n$ZWJ                = [\\p{Word_Break = ZWJ}];\n$Regional_Indicator = [\\p{Word_Break = Regional_Indicator}];\n$Format             = [\\p{Word_Break = Format}];\n$Katakana           = [\\p{Word_Break = Katakana}];\n$Hebrew_Letter      = [\\p{Word_Break = Hebrew_Letter}];\n$ALetter            = [\\p{Word_Break = ALetter} @];\n$Single_Quote       = [\\p{Word_Break = Single_Quote}];\n$Double_Quote       = [\\p{Word_Break = Double_Quote}];\n$MidNumLet          = [\\p{Word_Break = MidNumLet}];\n$MidLetter          = [\\p{Word_Break = MidLetter} - [\\: \\uFE55 \\uFF1A]];\n$MidNum             = [\\p{Word_Break = MidNum}];\n$Numeric            = [\\p{Word_Break = Numeric}];\n$ExtendNumLet       = [\\p{Word_Break = ExtendNumLet}];\n$WSegSpace          = [\\p{Word_Break = WSegSpace}];\n$Extended_Pict      = [\\p{Extended_Pictographic}];\n\n$Hiragana           = [:Hiragana:];\n$Ideographic        = [\\p{Ideographic}];\n\n\n#   Dictionary character set, for triggering language-based break engines. Currently\n#   limited to LineBreak=Complex_Context. Note that this set only works in Unicode\n#   5.0 or later as the definition of Complex_Context was corrected to include all\n#   characters requiring dictionary break.\n\n$Control        = [\\p{Grapheme_Cluster_Break = Control}];\n$HangulSyllable = [\\uac00-\\ud7a3];\n$ComplexContext = [:LineBreak = Complex_Context:];\n$KanaKanji      = [$Han $Hiragana $Katakana];\n$dictionaryCJK  = [$KanaKanji $HangulSyllable];\n$dictionary     = [$ComplexContext $dictionaryCJK];\n\n# TODO: check if handling of katakana in dictionary makes rules incorrect/void\n\n# leave CJK scripts out of ALetterPlus\n$ALetterPlus  = [$ALetter-$dictionaryCJK [$ComplexContext-$Extend-$Control]];\n\n\n## -------------------------------------------------\n\n# Rule 3 - CR x LF\n#\n$CR $LF;\n\n# Rule 3c   Do not break within emoji zwj sequences.\n#             ZWJ ×  \\p{Extended_Pictographic}.  Precedes WB4, so no intervening Extend chars allowed.\n#\n$ZWJ $Extended_Pict;\n\n# Rule 3d - Keep horizontal whitespace together.\n#\n$WSegSpace $WSegSpace;\n\n# Rule 4 - ignore Format and Extend characters, except when they appear at the beginning\n#          of a region of Text.\n\n$ExFm  = [$Extend $Format $ZWJ];\n\n^$ExFm+;            # This rule fires only when there are format or extend characters at the\n                    # start of text, or immediately following another boundary. It groups them, in\n                    # the event there are more than one.\n\n[^$CR $LF $Newline $ExFm] $ExFm*;   # This rule rule attaches trailing format/extends to words,\n                                    # with no special rule status value.\n\n$Numeric $ExFm* {100};              # This group of rules also attach trailing format/extends, but\n$ALetterPlus $ExFm* {200};          # with rule status set based on the word's final base character.\n$HangulSyllable {200};\n$Hebrew_Letter $ExFm* {200};\n$Katakana $ExFm* {400};             # note:  these status values override those from rule 5\n$Hiragana $ExFm* {400};             #        by virtue of being numerically larger.\n$Ideographic $ExFm* {400};          #\n\n#\n# rule 5\n#    Do not break between most letters.\n#\n($ALetterPlus | $Hebrew_Letter) $ExFm* ($ALetterPlus | $Hebrew_Letter);\n\n# rule 6 and 7\n($ALetterPlus | $Hebrew_Letter)  $ExFm* ($MidLetter | $MidNumLet | $Single_Quote) $ExFm* ($ALetterPlus | $Hebrew_Letter) {200};\n\n# rule 7a\n$Hebrew_Letter $ExFm* $Single_Quote {200};\n\n# rule 7b and 7c\n$Hebrew_Letter $ExFm* $Double_Quote $ExFm* $Hebrew_Letter;\n\n# rule 8\n\n$Numeric $ExFm* $Numeric;\n\n# rule 9\n\n($ALetterPlus | $Hebrew_Letter)  $ExFm* $Numeric;\n\n# rule 10\n\n$Numeric $ExFm* ($ALetterPlus | $Hebrew_Letter);\n\n# rule 11 and 12\n\n$Numeric $ExFm* ($MidNum | $MidNumLet | $Single_Quote) $ExFm* $Numeric;\n\n# rule 13\n# to be consistent with $KanaKanji $KanaKanhi, changed\n# from 300 to 400.\n# See also TestRuleStatus in intltest/rbbiapts.cpp\n$Katakana $ExFm*  $Katakana {400};\n\n# rule 13a/b\n\n$ALetterPlus   $ExFm* $ExtendNumLet {200};    #  (13a)\n$Hebrew_Letter $ExFm* $ExtendNumLet {200};    #  (13a)\n$Numeric       $ExFm* $ExtendNumLet {100};    #  (13a)\n$Katakana      $ExFm* $ExtendNumLet {400};    #  (13a)\n$ExtendNumLet  $ExFm* $ExtendNumLet {200};    #  (13a)\n\n$ExtendNumLet  $ExFm* $ALetterPlus  {200};    #  (13b)\n$ExtendNumLet  $ExFm* $Hebrew_Letter {200};    #  (13b)\n$ExtendNumLet  $ExFm* $Numeric      {100};    #  (13b)\n$ExtendNumLet  $ExFm* $Katakana     {400};    #  (13b)\n\n# rules 15 - 17\n#    Pairs of Regional Indicators stay together.\n#    With incoming rule chaining disabled by ^, this rule will match exactly two of them.\n#    No other rule begins with a Regional_Indicator, so chaining cannot extend the match.\n#\n^$Regional_Indicator $ExFm* $Regional_Indicator;\n\n# special handling for CJK characters: chain for later dictionary segmentation\n$HangulSyllable $HangulSyllable {200};\n$KanaKanji $KanaKanji {400}; # different rule status if both kana and kanji found\n\n# Rule 999\n#     Match a single code point if no other rule applies.\n.;"
#> 
#> $keep_hyphens
#> [1] "$Hyphen = [\\p{Pd}];\n($ALetterPlus | $Hebrew_Letter | $Numeric)  $ExFm* $Hyphen $ExFm* ($ALetterPlus | $Hebrew_Letter | $Numeric);\n"
#> 
#> $split_elisions
#> [1] "$Elision = ([lLmMtTnNsSjJdDcC]|([jJ]'us'|[qQ]'uoi'|[lL]'ors'|[pP]'uis'|[qQ]'uel')?[qQ]'u')[\\u0027\\u2019];\n^$Elision / $ALetterPlus;\n"
#> 
#> $split_tags
#> [1] "^[#] / [\\p{L}\\p{N}_]+ / [#]?; \n^[@] / [a-zA-Z0-9_]+;"
#> 
breakrules_get("sentence")
#> $base
#> [1] "# Copyright (C) 2016 and later: Unicode, Inc. and others.\n# License & terms of use: http://www.unicode.org/copyright.html\n#\n#   Copyright (C) 2002-2015, International Business Machines Corporation and others.\n#       All Rights Reserved.\n#\n#   file:  sent.txt\n#\n#   ICU Sentence Break Rules\n#      See Unicode Standard Annex #29.\n#      These rules are based on UAX #29 Revision 34 for Unicode Version 12.0\n#\n\n!!quoted_literals_only;\n\n#\n# Character categories as defined in TR 29\n#\n$CR        = [\\p{Sentence_Break = CR}];\n$LF        = [\\p{Sentence_Break = LF}];\n$Extend    = [\\p{Sentence_Break = Extend}];\n$Sep       = [\\p{Sentence_Break = Sep}];\n$Format    = [\\p{Sentence_Break = Format}];\n$Sp        = [\\p{Sentence_Break = Sp}];\n$Lower     = [\\p{Sentence_Break = Lower}];\n$Upper     = [\\p{Sentence_Break = Upper}];\n$OLetter   = [\\p{Sentence_Break = OLetter}];\n$Numeric   = [\\p{Sentence_Break = Numeric}];\n$ATerm     = [\\p{Sentence_Break = ATerm}];\n$SContinue = [\\p{Sentence_Break = SContinue}];\n$STerm     = [\\p{Sentence_Break = STerm}];\n$Close     = [\\p{Sentence_Break = Close}];\n\n#\n# Define extended forms of the character classes,\n#   incorporate trailing Extend or Format chars.\n#   Rules 4 and 5.\n\n$SpEx       = $Sp      ($Extend | $Format)*;\n$LowerEx    = $Lower   ($Extend | $Format)*;\n$UpperEx    = $Upper   ($Extend | $Format)*;\n$OLetterEx  = $OLetter ($Extend | $Format)*;\n$NumericEx  = $Numeric ($Extend | $Format)*;\n$ATermEx    = $ATerm   ($Extend | $Format)*;\n$SContinueEx= $SContinue ($Extend | $Format)*;\n$STermEx    = $STerm   ($Extend | $Format)*;\n$CloseEx    = $Close   ($Extend | $Format)*;\n\n\n## -------------------------------------------------\n\n!!chain;\n\n# Rule 3 - break after separators.  Keep CR/LF together.\n#\n$CR $LF;\n\n\n# Rule 4 - Break after $Sep.\n# Rule 5 - Ignore $Format and $Extend\n#\n[^$Sep $CR $LF]? ($Extend | $Format)*;\n\n\n# Rule 6\n$ATermEx $NumericEx;\n\n# Rule 7\n($UpperEx | $LowerEx) $ATermEx $UpperEx;\n\n#Rule 8\n$NotLettersEx = [^$OLetter $Upper $Lower $Sep $CR $LF $ATerm $STerm] ($Extend | $Format)*;\n$ATermEx $CloseEx* $SpEx* $NotLettersEx* $Lower;\n\n# Rule 8a\n($STermEx | $ATermEx) $CloseEx* $SpEx* ($SContinueEx | $STermEx | $ATermEx);\n\n#Rule 9, 10, 11\n($STermEx | $ATermEx) $CloseEx* $SpEx* ($Sep | $CR | $LF)?;\n\n#Rule 998\n[[^$STerm $ATerm $Close $Sp $Sep $LF $CR $Format $Extend]{bof}] ($Extend | $Format | $Close | $Sp)* .;\n[[^$STerm $ATerm $Close $Sp $Sep $LF $CR $Format $Extend]{bof}] ($Extend | $Format | $Close | $Sp)* ([$Sep $LF $CR {eof}] | $CR $LF){100};"
#> 

brw <- breakrules_get("word")
brw$keep_email <- "@[a-zA-Z0-9_]+"
breakrules_set(brw, what = "word")
breakrules_reset("sentence")
breakrules_reset("word")
```
