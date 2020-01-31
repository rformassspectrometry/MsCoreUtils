

test_that("function: all normalize methods", {
    for (.method in normalizeMethods()) {
        x_nrom <- normalize(x, method = .method)
        expect_identical(dim(x_norm), dim(x))
    }
})
