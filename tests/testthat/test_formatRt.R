test_that("rt2numeric()", {
    tc <- c("1:01", "25:24")
    tn <- c(61, 25 * 60 + 24)
    expect_true(is.na(rt2numeric("")))
    expect_warning(is.na(rt2numeric("aaa")))
    expect_equal(tc, rt2character(tn))
    expect_error(rt2numeric(numeric()))
    expect_error(rt2numeric(character()))
    expect_error(rt2numeric(list()))
    expect_equal(rt2numeric(c("25:24", "25:25", "25:26")),
                 c(1524:1526))
})


test_that("rt2character()", {    
    tn <- c(61, 25 * 60 + 24)
    tc <- c("1:01", "25:24")
    expect_equal(tn, rt2numeric(tc))
    expect_error(is.na(rt2character(TRUE)))
    expect_equal(rt2character(1), "0:01")
    expect_equal(rt2character(60), "1:00")
    expect_equal(rt2character(61), "1:01")
    expect_error(rt2character(numeric()))
    expect_error(rt2character(character()))
    expect_error(rt2character(list()))
    expect_identical(c("0:01", "0:02", "0:03", "0:04", "0:05", "0:06",
                       "0:07", "0:08", "0:09", "0:10"),
                     rt2character(1:10))
})


test_that("formatRt()", {
    tc <- c("1:01", "25:24")
    tn <- c(61, 25 * 60 + 24)
    expect_equal(tc, formatRt(tn))
    expect_equal(tn, formatRt(tc))
    expect_true(is.na(formatRt("")))
    expect_warning(is.na(formatRt("aaa")))
    expect_error(formatRt(TRUE))
    expect_equal(formatRt(1), "0:01")
    expect_equal(formatRt(60), "1:00")
    expect_equal(formatRt(61), "1:01")
    expect_error(formatRt(numeric()))
    expect_error(formatRt(character()))
    expect_error(formatRt(list()))
    expect_identical(c("0:01", "0:02", "0:03", "0:04", "0:05", "0:06",
                       "0:07", "0:08", "0:09", "0:10"),
                     formatRt(1:10))
    expect_equal(formatRt(c("25:24", "25:25", "25:26")),
                 c(1524:1526))
})
