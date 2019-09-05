test_that(".validateWindow", {
    expect_error(.validateWindow(3, 10L), "integer")
    expect_error(.validateWindow(3L:4L, 10L), "length")
    expect_error(.validateWindow(-1L, 10L), "larger")
    expect_error(.validateWindow(11L, 10L), "smaller")
    expect_true(.validateWindow(3L, 10L))
})
