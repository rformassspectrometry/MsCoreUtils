test_that("logMessage", {
    expect_equal(logMessage(NULL, "foo"), paste0("foo [", date(), "]"))
    expect_equal(logMessage(NULL, "foo", date = FALSE), "foo")
    expect_equal(logMessage("foo", "bar"),
                 c("foo", paste0("bar [", date(), "]")))
})
