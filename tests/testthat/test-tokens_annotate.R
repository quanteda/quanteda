txt <- c(d1 = "The Atlantic Ocean and the Pacific Ocean.",
         d2 = "The Supreme Court of the United States",
         d3 = "Arsenal versus Manchester United")

toks <- tokens(txt, remove_punct = TRUE, padding = TRUE) %>% 
    tokens_remove(stopwords(), padding = TRUE) %>% 
    tokens_tolower()

dict <- dictionary(list(countries = c("United States"),
                        oceans = c("Atlantic Ocean", "Pacific Ocean"),
                        institutions = c("Supreme Court"),
                        team = list(football = c("Manchester United", "Arsenal"))))

test_that("tokens_annotate works()", {
    
    
    expect_equal(
        as.list(tokens_annotate(toks, dict)),
        list(d1 = c("", "atlantic", "ocean", "#OCEANS#", "", "", "pacific", "ocean", "#OCEANS#", ""),
             d2 = c("", "supreme", "court", "#INSTITUTIONS#", "", "", "united", "states", "#COUNTRIES#"),
             d3 = c("arsenal", "#TEAM.FOOTBALL#", "versus", "manchester", "united", "#TEAM.FOOTBALL#"))
    )
    expect_equal(
        as.list(tokens_annotate(toks, dict, capkey = FALSE)),
        list(d1 = c("", "atlantic", "ocean", "#oceans#", "", "", "pacific", "ocean", "#oceans#", ""),
             d2 = c("", "supreme", "court", "#institutions#", "", "", "united", "states", "#countries#"),
             d3 = c("arsenal", "#team.football#", "versus", "manchester", "united", "#team.football#"))
    )

    expect_equal(
        as.list(tokens_annotate(toks, dict, marker = c("<", ">"))),
        list(d1 = c("", "atlantic", "ocean", "<OCEANS>", "", "", "pacific", "ocean", "<OCEANS>", ""),
             d2 = c("", "supreme", "court", "<INSTITUTIONS>", "", "", "united", "states", "<COUNTRIES>"),
             d3 = c("arsenal", "<TEAM.FOOTBALL>", "versus", "manchester", "united", "<TEAM.FOOTBALL>"))
    )
    
    expect_equal(
        as.list(tokens_annotate(toks, dict, levels = 1)),
        list(d1 = c("", "atlantic", "ocean", "#OCEANS#", "", "", "pacific", "ocean", "#OCEANS#", ""),
             d2 = c("", "supreme", "court", "#INSTITUTIONS#", "", "", "united", "states", "#COUNTRIES#"),
             d3 = c("arsenal", "#TEAM#", "versus", "manchester", "united", "#TEAM#"))
    )
    
    expect_equal(
        as.list(tokens_annotate(toks, dict, levels = 2)),
        list(d1 = c("", "atlantic", "ocean", "", "", "pacific", "ocean", ""),
             d2 = c("", "supreme", "court", "", "", "united", "states"),
             d3 = c("arsenal", "#FOOTBALL#", "versus", "manchester", "united", "#FOOTBALL#"))
    )
    
    expect_error(
        tokens_annotate(toks, "pattern*"),
        "dictionary must be a dictionary object"
    )
    
    expect_error(
        tokens_annotate(toks, dict, marker = character()),
        "The length of marker must be between 1 and 2"
    )
    
})
