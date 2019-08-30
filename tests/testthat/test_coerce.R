test_that("asInteger works", {
    expect_true(is.integer(asInteger(2.3)))
    expect_error(asInteger("a"))
    myvar <- "2"
    expect_error(asInteger(myvar), "Argument myvar should be")
})
