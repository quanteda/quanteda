citHeader("To cite package 'quanteda' in publications please use the following.")

if (!exists("meta") || is.null(meta)) 
    meta <- packageDescription("quanteda")

citEntry(entry = "Manual",
         title = "quanteda: Quantitative Analysis of Textual Data",
         author = personList(as.person("Kenneth Benoit"),
                             as.person("Paul Nulty")),                            
         year = substring(meta$Date, 1, 4),
         note = sprintf("R package version %s", meta$Version),
         url = "https://github.com/kbenoit/quanteda",
         
         textVersion = 
             sprintf("Benoit, Kenneth and Paul Nulty.  (%s).  \"quanteda: Quantitative Analysis of 
                     Textual Data\".  R package version: %s.  URL https://github.com/kbenoit/quanteda", 
                     year, meta$Version)
)
