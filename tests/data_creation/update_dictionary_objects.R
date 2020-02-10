# add metadata to dictionary objects

meta(data_dictionary_LSD2015) <-
    list(title = "Lexicoder Sentiment Dictionary (2015)",
         description = 'The 2015 Lexicoder Sentiment Dictionary in quanteda dictionary format.  

The dictionary consists of 2,858 "negative" sentiment words and 1,709 "positive" sentiment words. A further set of 2,860 and 1,721 negations of negative and positive words, respectively, is also included.  The dictionary was developed by Lori Young and Stuart Soroka (2012).  Its  objectives, development and reliability are discussed in detail in Young and Soroka (2012). 

Please cite this article when using the Lexicoder Sentiment Dictionary and related resources. Young, L. & Soroka, S. (2012). Lexicoder Sentiment Dictionary. Available at http://lexicoder.com.
',
         source = "Young, L. & Soroka, S. (2012). Affective News: The Automated Coding of Sentiment in Political Texts. Political Communication, 29(2), 205–231.",
         url = "http://lexicoder.com",
         license = "The LSD is available for non-commercial academic purposes only. By using data_dictionary_LSD2015, you accept these terms.  Please cite the references below when using the dictionary."
    )
usethis::use_data(data_dictionary_LSD2015, overwrite = TRUE)         
