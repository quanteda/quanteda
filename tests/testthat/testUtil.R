test_that("Test quanteda:::mktemp function for test dirs",{
    filename <- quanteda:::mktemp()
    expect_true(file.exists(filename))
    filename2 <- quanteda:::mktemp()
    expect_true(file.exists(filename2))
    expect_false(filename == filename2)
    
    # test directory parameter
    dirname <- quanteda:::mktemp(directory=T)
    expect_true(dir.exists(dirname))
    
    # test prefix parameter
    filename <- quanteda:::mktemp(prefix='testprefix')
    expect_equal(
        substr(basename(filename), 1, 10),
        'testprefix'
    )
    
    # test that a new filename will be given if the original already exists
    set.seed(0)
    original_filename <- quanteda:::mktemp()
    set.seed(0)
    new_filename <- quanteda:::mktemp()
    expect_false(original_filename == new_filename)
    expect_true(file.exists(original_filename))
    expect_true(file.exists(new_filename))
    
    
})

