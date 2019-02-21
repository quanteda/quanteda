context("test meta")

field_system <- c("source", "package-version", "r-version", "system", "directory", "created")

test_that("meta works", {
    
    corp <- corpus(c(d1 = "one two three", d2 = "two three four"))
    
    expect_equal(names(attr(corp, "meta")$system), field_system)
    expect_equal(quanteda:::meta_system(corp, "source"), "character")
    expect_equal(class(quanteda:::meta_system(corp, "package-version")), 
                 c("package_version", "numeric_version"))
    expect_equal(class(quanteda:::meta_system(corp, "r-version")), 
                 c("R_system_version", "package_version", "numeric_version"))
    expect_equal(class(quanteda:::meta_system(corp, "created")), "Date")

    quanteda:::meta_system(corp, "something") <- "somevalue"
    expect_equal(quanteda:::meta_system(corp, "something"), "somevalue")
    expect_equal(quanteda:::meta_system(corp, "nothing"), NULL)
})

