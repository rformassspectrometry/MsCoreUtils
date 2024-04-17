test_that("common_path works", {
    expect_equal(common_path(character()), character())
    expect_equal(common_path(c("a.txt", "b.txt")), "")
    expect_equal(common_path(c("common/path/a.txt", "common/path/b.txt")),
                 "common/path")
    expect_equal(common_path(c("/first/second/a.txt",
                               "/first/second/third/b.txt")),
                 "/first/second")
    ## Windows paths (\\ is used by `normalizePaths`)
    x <- c("C:\\first\\second\\a.txt", "C:\\first\\second\\b.txt")
    expect_equal(common_path(x), "C:/first/second")
    x <- c("C:\\first\\second\\a.txt", "C:\\first\\second\\third\\b.txt")
    expect_equal(common_path(x), "C:/first/second")
})
