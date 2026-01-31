
test_that("dictionary_tokenize is working", {
    
    dict1 <- dictionary(list(ASIA = list(
        "IN" = "印度", 
        "ID" = "印度尼西亚"
    )))
    expect_equivalent(
        dictionary_tokenize(dict1),
        list(ASIA = list("IN" = "印度", 
                         "ID" = "印度 尼西亚"))
    )
    
    # with punct
    dict2 <- dictionary(list(AFRICA = list(
        "CD" ="コンゴ民主共和国",
        "CF" = "中央アフリカ",
        "CI" = "コート・ジボワール"
    )))
    
    expect_equivalent(
        dictonary_tokenize(dict2, remove_punct = FALSE),
        list(ASIA = list(
            "CD" ="コンゴ 民主 共和国",
            "CF" = "中央 アフリカ",
            "CI" = "コート ・ ジボワール"
        ))
    )
    expect_equivalent(
        dictonary_tokenize(dict2, remove_punct = TRUE),
        list(ASIA = list(
            "CD" ="コンゴ 民主 共和国",
            "CF" = "中央 アフリカ",
            "CI" = "コート ジボワール"
        ))
    )
    expect_equivalent(
        dictonary_tokenize(dict2, remove_punct = TRUE, padding = TRUE),
        list(ASIA = list(
            "CD" ="コンゴ 民主 共和国",
            "CF" = "中央 アフリカ",
            "CI" = "コート  ジボワール"
        ))
    )
    
    # with tags
    quanteda_options(pattern_username = "@[\\w]+")
    dict3 <- dictionary(list(olympic = c(
        "#東京五輪", "@都知事", "東京オリンピック"
    )))
    expect_equivalent(
        dictonary_tokenize(dict3),
        list(
            "olympic" = c("#東京五輪", "@都知事", "東京 オリンピック")
        )
    )
    expect_equivalent(
        dictonary_tokenize(dict3, split_tags = TRUE),
        list(
            "olympic" = c("# 東京五輪", "@ 都知事", "東京 オリンピック")
        )
    )
    quanteda_options(reset = TRUE)

})


