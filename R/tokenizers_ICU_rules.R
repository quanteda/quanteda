
## Default word rules from ICU
base_word_rules <-
  # readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/word.txt") |>
  # paste0(collapse = "\n")
r"---(
#
# Copyright (C) 2016 and later: Unicode, Inc. and others.
# License & terms of use: http://www.unicode.org/copyright.html
# Copyright (C) 2002-2016, International Business Machines Corporation
# and others. All Rights Reserved.
#
# file:  word.txt
#
# ICU Word Break Rules
#      See Unicode Standard Annex #29.
#      These rules are based on UAX #29 Revision 34 for Unicode Version 12.0
#
# Note:  Updates to word.txt will usually need to be merged into
#        word_POSIX.txt also.

##############################################################################
#
#  Character class definitions from TR 29
#
##############################################################################

!!chain;
!!quoted_literals_only;


#
#  Character Class Definitions.
#

$Han                = [:Han:];

$CR                 = [\p{Word_Break = CR}];
$LF                 = [\p{Word_Break = LF}];
$Newline            = [\p{Word_Break = Newline}];
$Extend             = [\p{Word_Break = Extend}-$Han];
$ZWJ                = [\p{Word_Break = ZWJ}];
$Regional_Indicator = [\p{Word_Break = Regional_Indicator}];
$Format             = [\p{Word_Break = Format}];
$Katakana           = [\p{Word_Break = Katakana}];
$Hebrew_Letter      = [\p{Word_Break = Hebrew_Letter}];
$ALetter            = [\p{Word_Break = ALetter}];
$Single_Quote       = [\p{Word_Break = Single_Quote}];
$Double_Quote       = [\p{Word_Break = Double_Quote}];
$MidNumLet          = [\p{Word_Break = MidNumLet}];
$MidLetter          = [\p{Word_Break = MidLetter}];
$MidNum             = [\p{Word_Break = MidNum}];
$Numeric            = [\p{Word_Break = Numeric}];
$ExtendNumLet       = [\p{Word_Break = ExtendNumLet}];
$WSegSpace          = [\p{Word_Break = WSegSpace}];
$Extended_Pict      = [\p{Extended_Pictographic}];

$Hiragana           = [:Hiragana:];
$Ideographic        = [\p{Ideographic}];


#   Dictionary character set, for triggering language-based break engines. Currently
#   limited to LineBreak=Complex_Context. Note that this set only works in Unicode
#   5.0 or later as the definition of Complex_Context was corrected to include all
#   characters requiring dictionary break.

$Control        = [\p{Grapheme_Cluster_Break = Control}];
$HangulSyllable = [\uac00-\ud7a3];
$ComplexContext = [:LineBreak = Complex_Context:];
$KanaKanji      = [$Han $Hiragana $Katakana];
$dictionaryCJK  = [$KanaKanji $HangulSyllable];
$dictionary     = [$ComplexContext $dictionaryCJK];

# TODO: check if handling of katakana in dictionary makes rules incorrect/void

# leave CJK scripts out of ALetterPlus
$ALetterPlus  = [$ALetter-$dictionaryCJK [$ComplexContext-$Extend-$Control]];


## -------------------------------------------------

# Rule 3 - CR x LF
#
$CR $LF;

# Rule 3c   Do not break within emoji zwj sequences.
#             ZWJ Ã—  \p{Extended_Pictographic}.  Precedes WB4, so no intervening Extend chars allowed.
#
$ZWJ $Extended_Pict;

# Rule 3d - Keep horizontal whitespace together.
#
$WSegSpace $WSegSpace;

# Rule 4 - ignore Format and Extend characters, except when they appear at the beginning
#          of a region of Text.

$ExFm  = [$Extend $Format $ZWJ];

^$ExFm+;            # This rule fires only when there are format or extend characters at the
# start of text, or immediately following another boundary. It groups them, in
# the event there are more than one.

[^$CR $LF $Newline $ExFm] $ExFm*;   # This rule rule attaches trailing format/extends to words,
# with no special rule status value.

$Numeric $ExFm* {100};              # This group of rules also attach trailing format/extends, but
$ALetterPlus $ExFm* {200};          # with rule status set based on the word's final base character.
$HangulSyllable {200};
$Hebrew_Letter $ExFm* {200};
$Katakana $ExFm* {400};             # note:  these status values override those from rule 5
$Hiragana $ExFm* {400};             #        by virtue of being numerically larger.
$Ideographic $ExFm* {400};          #

#
# rule 5
#    Do not break between most letters.
#
($ALetterPlus | $Hebrew_Letter) $ExFm* ($ALetterPlus | $Hebrew_Letter);

# rule 6 and 7
($ALetterPlus | $Hebrew_Letter)  $ExFm* ($MidLetter | $MidNumLet | $Single_Quote) $ExFm* ($ALetterPlus | $Hebrew_Letter) {200};

# rule 7a
$Hebrew_Letter $ExFm* $Single_Quote {200};

# rule 7b and 7c
$Hebrew_Letter $ExFm* $Double_Quote $ExFm* $Hebrew_Letter;

# rule 8

$Numeric $ExFm* $Numeric;

# rule 9

($ALetterPlus | $Hebrew_Letter)  $ExFm* $Numeric;

# rule 10

$Numeric $ExFm* ($ALetterPlus | $Hebrew_Letter);

# rule 11 and 12

$Numeric $ExFm* ($MidNum | $MidNumLet | $Single_Quote) $ExFm* $Numeric;

# rule 13
# to be consistent with $KanaKanji $KanaKanhi, changed
# from 300 to 400.
# See also TestRuleStatus in intltest/rbbiapts.cpp
$Katakana $ExFm*  $Katakana {400};

# rule 13a/b

$ALetterPlus   $ExFm* $ExtendNumLet {200};    #  (13a)
$Hebrew_Letter $ExFm* $ExtendNumLet {200};    #  (13a)
$Numeric       $ExFm* $ExtendNumLet {100};    #  (13a)
$Katakana      $ExFm* $ExtendNumLet {400};    #  (13a)
$ExtendNumLet  $ExFm* $ExtendNumLet {200};    #  (13a)

$ExtendNumLet  $ExFm* $ALetterPlus  {200};    #  (13b)
$ExtendNumLet  $ExFm* $Hebrew_Letter {200};    #  (13b)
$ExtendNumLet  $ExFm* $Numeric      {100};    #  (13b)
$ExtendNumLet  $ExFm* $Katakana     {400};    #  (13b)

# rules 15 - 17
#    Pairs of Regional Indicators stay together.
#    With incoming rule chaining disabled by ^, this rule will match exactly two of them.
#    No other rule begins with a Regional_Indicator, so chaining cannot extend the match.
#
^$Regional_Indicator $ExFm* $Regional_Indicator;

# special handling for CJK characters: chain for later dictionary segmentation
$HangulSyllable $HangulSyllable {200};
$KanaKanji $KanaKanji {400}; # different rule status if both kana and kanji found

# Rule 999
#     Match a single code point if no other rule applies.
.;
)---"

  
## Default sentence rules from ICU
base_sentence_rules <-
  # readLines("https://raw.githubusercontent.com/unicode-org/icu/main/icu4c/source/data/brkitr/rules/sent.txt") |>
  # paste0(collapse = "\n")
r"---(
# Copyright (C) 2016 and later: Unicode, Inc. and others.
# License & terms of use: http://www.unicode.org/copyright.html
#
#   Copyright (C) 2002-2015, International Business Machines Corporation and others.
#       All Rights Reserved.
#
#   file:  sent.txt
#
#   ICU Sentence Break Rules
#      See Unicode Standard Annex #29.
#      These rules are based on UAX #29 Revision 34 for Unicode Version 12.0
#

!!quoted_literals_only;

#
# Character categories as defined in TR 29
#
$CR        = [\p{Sentence_Break = CR}];
$LF        = [\p{Sentence_Break = LF}];
$Extend    = [\p{Sentence_Break = Extend}];
$Sep       = [\p{Sentence_Break = Sep}];
$Format    = [\p{Sentence_Break = Format}];
$Sp        = [\p{Sentence_Break = Sp}];
$Lower     = [\p{Sentence_Break = Lower}];
$Upper     = [\p{Sentence_Break = Upper}];
$OLetter   = [\p{Sentence_Break = OLetter}];
$Numeric   = [\p{Sentence_Break = Numeric}];
$ATerm     = [\p{Sentence_Break = ATerm}];
$SContinue = [\p{Sentence_Break = SContinue}];
$STerm     = [\p{Sentence_Break = STerm}];
$Close     = [\p{Sentence_Break = Close}];

#
# Define extended forms of the character classes,
#   incorporate trailing Extend or Format chars.
#   Rules 4 and 5.

$SpEx       = $Sp      ($Extend | $Format)*;
$LowerEx    = $Lower   ($Extend | $Format)*;
$UpperEx    = $Upper   ($Extend | $Format)*;
$OLetterEx  = $OLetter ($Extend | $Format)*;
$NumericEx  = $Numeric ($Extend | $Format)*;
$ATermEx    = $ATerm   ($Extend | $Format)*;
$SContinueEx= $SContinue ($Extend | $Format)*;
$STermEx    = $STerm   ($Extend | $Format)*;
$CloseEx    = $Close   ($Extend | $Format)*;


## -------------------------------------------------

!!chain;

# Rule 3 - break after separators.  Keep CR/LF together.
#
$CR $LF;


# Rule 4 - Break after $Sep.
# Rule 5 - Ignore $Format and $Extend
#
[^$Sep $CR $LF]? ($Extend | $Format)*;


# Rule 6
$ATermEx $NumericEx;

# Rule 7
($UpperEx | $LowerEx) $ATermEx $UpperEx;

#Rule 8
$NotLettersEx = [^$OLetter $Upper $Lower $Sep $CR $LF $ATerm $STerm] ($Extend | $Format)*;
$ATermEx $CloseEx* $SpEx* $NotLettersEx* $Lower;

# Rule 8a
($STermEx | $ATermEx) $CloseEx* $SpEx* ($SContinueEx | $STermEx | $ATermEx);

#Rule 9, 10, 11
($STermEx | $ATermEx) $CloseEx* $SpEx* ($Sep | $CR | $LF)?;

#Rule 998
[[^$STerm $ATerm $Close $Sp $Sep $LF $CR $Format $Extend]{bof}] ($Extend | $Format | $Close | $Sp)* .;
[[^$STerm $ATerm $Close $Sp $Sep $LF $CR $Format $Extend]{bof}] ($Extend | $Format | $Close | $Sp)* ([$Sep $LF $CR {eof}] | $CR $LF){100};
)---"



# Custom rules ------------------------------------------------------------

## Hyphen rule
hyphen_rule_lax <- 
r"---(
$Hyphen = [\p{Pd}];
($ALetterPlus | $Hebrew_Letter | $Numeric)  $ExFm* $Hyphen $ExFm* ($ALetterPlus | $Hebrew_Letter | $Numeric)? {200};
)---"

## Hyphen rule
hyphen_rule <- 
r"---(
$Hyphen = [\p{Pd}];
($ALetterPlus | $Hebrew_Letter | $Numeric)  $ExFm* $Hyphen $ExFm* ($ALetterPlus | $Hebrew_Letter | $Numeric) {200};
)---"

## URL and email rule
url_rule <- 
r"---(
# url
^(([h][t][t][p][s]?\:\/\/([w][w][w]\.)?)|([w][w][w]\.))[-a-zA-Z0-9@:%_\+\.~#=]+(\/[-a-zA-Z0-9@\:%_\+\.~#?\&=]+)*[\/]?;
# email
[A-Za-z0-9_]+\@[A-Za-z][A-Za-z0-9_]+\.[a-z]+;
)---"


### Protect variant selector & whitespace with diacritical marks
variantSelector_diacritical <- r"-(
$Variant = [\uFE00-\uFE0F];
$Diacritical = [\p{whitespace}][\u0300-\u036F];

# Rules
($ALetterPlus | $Hebrew_Letter) $Variant ($ALetterPlus | $Hebrew_Letter);
($ALetterPlus | $Hebrew_Letter) $Diacritical ($ALetterPlus | $Hebrew_Letter);
)-"

# Elision definition and rule
Elision_french <- "
$Elision = ([lLmMtTnNsSjJdDcC]|([jJ][u][s]|[qQ][u][o][i]|[lL][o][r][s]|[pP][u][i][s])?[qQ][u])[\u0027\u2019];
# Disable chaining so it only matches beginning of word.
^$Elision / $ALetterPlus;
"

