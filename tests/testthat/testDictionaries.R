
# require(quanteda)
# tmpdic2007 <- dictionary(file="~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2007_English080730.dic", format='LIWC')
# tmpdic2001 <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic", format = "LIWC")
# 
# tmpmfdic <- dictionary(file = "~/Dropbox/QUANTESS/dictionaries/Moral Foundations dictionary/moral_foundations_dictionary.dic", format = "LIWC")
# 
# lengths(tmpdic2001)
# # Pronoun       I      We    Self     You   Other  Negate  Assent Article   Preps  Number  Affect  Posemo Posfeel   Optim  Negemo     Anx   Anger     Sad Cogmech   Cause Insight Discrep   Inhib 
# # 72       9      11      20      14      22      31      18       3      43      29     617     263      42      70     345      61     120      72     321      51     117      32      64 
# # Tentat Certain  Senses     See    Hear    Feel  Social    Comm  Othref Friends  Family  Humans    Time    Past Present  Future   Space      Up    Down    Incl    Excl  Motion   Occup  School 
# # 80      30     112      32      36      29     325     130      54      29      47      43     113     145     261      14      74      12       7      16      19      73     215     100 
# # Job Achieve Leisure    Home  Sports      TV   Music   Money  Metaph   Relig   Death Physcal    Body  Sexual  Eating   Sleep   Groom   Swear   Nonfl Fillers 
# # 63      61     103      26      29      19      31      75      85      56      29     297     202      49      52      21      15      32       5      10 
# lengths(tmpdic2007)
# # funct pronoun   ppron       i      we     you   shehe    they   ipron article    verb auxverb    past present  future  adverb   preps    conj  negate   quant  number   swear  social  family 
# # 467     117      71      12      12      20      16      10      46       4     383     147     145     169      48      68      60      28      57      89      34      54       0       0 
# # friend  humans  affect  posemo  negemo     anx   anger     sad cogmech insight   cause discrep  tentat certain   inhib    incl    excl percept     see    hear    feel     bio    body  health 
# # 0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0 
# # sexual  ingest relativ  motion   space    time    work achieve leisure    home   money   relig   death  assent   nonfl  filler    <NA> 
# #     0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       1 
# 
# test_that("These should be non-zero in length", {
#     expect_more_than(length(tmpdic2001[["Number"]]), 0)
#     expect_more_than(length(tmpdic2007[["number"]]), 0)
#     expect_more_than(length(tmpdic2001[["Affect"]]), 0)
#     expect_more_than(length(tmpdic2007[["social"]]), 0)
# })
# 
# test_that("These should be non-zero in length", {
#     expect_true(all(lengths(tmpdic2001)))
#     expect_true(all(lengths(tmpdic2007)))
# })
# 
