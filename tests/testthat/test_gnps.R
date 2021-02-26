test_that("join_gnps works", {
    a <- cbind(mz = c(10, 36, 63, 91, 93),
               intensity = c(14, 15, 999, 650, 1))
    a_pmz <- 91

    b <- cbind(mz = c(10, 12, 50, 63, 105),
               intensity = c(35, 5, 16, 999, 450))
    b_pmz <- 105

    expect_equal(join_gnps(a[, 1L], b[, 1L]), join(a[, 1L], b[, 1L]))
    expect_equal(join_gnps(a[, 1L], b[, 1L], xPrecursorMz = 91),
                 join(a[, 1L], b[, 1L]))
    expect_equal(join_gnps(a[, 1L], b[, 1L], xPrecursorMz = 3,
                           yPrecursorMz = 3),
                 join(a[, 1L], b[, 1L]))

    res <- join_gnps(a[, 1L], b[, 1L], xPrecursorMz = a_pmz,
                     yPrecursorMz = b_pmz, type = "left")
    expect_true(length(res$x) > nrow(a))  # peaks can match multiple
    ## peak 36 matches no peak in b directly, but matches peak 50 in b
    ## considering the precursor difference
    expect_true(sum(res$x == 2) == 2)

    res_2 <- join_gnps(b[, 1L], a[, 1L], xPrecursorMz = b_pmz,
                       yPrecursorMz = a_pmz, type = "left")
    expect_equal(res_2$x, c(1, 2, 3, 3, 4, 5, 5))

    ## example with multi matches.
    a <- cbind(mz = c(10, 26, 36, 63, 91, 93),
               intensity = c(14, 10, 15, 999, 650, 1))
    a_pmz <- 91

    b <- cbind(mz = c(10, 12, 24, 50, 63, 105),
               intensity = c(35, 10, 5, 16, 999, 450))
    b_pmz <- 105

    res <- join_gnps(a[, 1L], b[, 1L], xPrecursorMz = a_pmz,
                     yPrecursorMz = b_pmz, type = "left")
    expect_equal(res$x, c(1, 1, 2, 3, 3, 4, 5, 5, 6))
    expect_equal(res$y, c(1, 3, NA, NA, 4, 5, NA, 6, NA))

    ## Example with peaks from a matching multiple peaks in b and vice versa
    a <- cbind(mz = c(10, 12, 14, 16, 19),
               intensity = 1:5)
    a_pmz <- 4
    b <- cbind(mz = c(10, 14, 16, 17, 19, 22),
               intensity = 1:6)
    b_pmz <- 8

    res <- join_gnps(a[, 1L], b[, 1L], a_pmz, b_pmz, type = "outer")
    expect_equal(res$x, c(1, 1, 2, 2, 3, 4, 5, NA, NA))
    expect_equal(res$y, c(1, 2, NA, 3, 2, 3, 5, 4, 6))
})

test_that("gnps works", {
    x <- cbind(mz = c(10, 12, 14, 16, 19),
               intensity = 1:5)
    xpmz <- 4
    y <- cbind(mz = c(10, 14, 16, 17, 19, 22),
               intensity = 1:6)
    ypmz <- 8

    expect_error(gnps(x, y), "aligned")
    a <- cbind(c(1, 2), c(0, 0))
    expect_equal(gnps(a, a), 0)
    a <- rbind(c(NA, NA), a)
    b <- a
    b[2:3, 1] <- NA
    expect_equal(gnps(a, b), 0)

    map <- join_gnps(x[, 1L], y[, 1L], xpmz, ypmz)
    res <- gnps(x[map$x, ], y[map$y, ])
    expect_true(res > 0.5)
    map <- join_gnps(y[, 1L], x[, 1L], ypmz, xpmz)
    res_2 <- gnps(y[map$x, ], x[map$y, ])
    expect_equal(res, res_2)

    ## Unit tests with reference values from GNPS online kindly provided by
    ## Liesa Salzer
    apmz <- 184.097
    a <- cbind(
        mz = c(59.049, 82.065002, 100.075996, 110.059998, 124.075996,
               125.059998, 142.085999),
        intensity = c(346.036011, 705.127014, 752.387024, 2193.239014,
                      2194.920898, 2455.741943, 3176.063965))
    bpmz <- 202.108
    b <- cbind(
        mz = c(55.018002, 59.049, 82.065002, 84.044998, 100.075996, 110.059998,
               125.059998, 142.085999, 152.070999, 156.065002),
        intensity = c(163.115997, 788.68103, 345.580994, 298.221008, 556.021973,
                      1938.749023, 1202.94104, 1801.137939, 1728.411011,
                      77.516998))
    map <- join_gnps(a[, 1L], b[, 1L], apmz, bpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], b[map$y, ]), 2), 0.76)
    mapr <- join_gnps(b[, 1L], a[, 1L], bpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], b[map$y, ]), gnps(b[mapr$x, ], a[mapr$y, ]))

    dpmz <- 220.12
    d <- cbind(
        mz = c(58.064999, 59.049, 60.060001, 82.065002, 100.075996, 110.059998,
               125.059998, 142.085999, 152.070999, 161.059998),
        intensity = c(49.41, 319.764008, 76.835999, 180.686005, 278.931,
                      969.950012, 653.91803, 1037.552979, 1123.036987,
                      245.953003))
    map <- join_gnps(a[, 1L], d[, 1L], apmz, dpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], d[map$y, ]), 2), 0.74)
    mapr <- join_gnps(d[, 1L], a[, 1L], dpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], d[map$y, ]), gnps(d[mapr$x, ], a[mapr$y, ]))

    map <- join_gnps(b[, 1L], d[, 1L], bpmz, dpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(b[map$x, ], d[map$y, ]), 2), 0.93)
    mapr <- join_gnps(d[, 1L], b[, 1L], dpmz, bpmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(b[map$x, ], d[map$y, ]), gnps(d[mapr$x, ], b[mapr$y, ]))

    ## Second set.
    apmz <- 371.564
    a <- cbind(
        mz = c(73.028999, 80.055, 87.043999, 89.059998, 103.039001, 133.085999,
               147.065002, 177.112, 195.121994, 207.123001, 221.138, 239.149002,
               279.144012, 283.174988, 309.156006, 327.201996, 354.225006,
               415.257996, 425.276001, 443.290009, 459.277008, 469.303986,
               487.311005, 503.30899, 531.336975, 547.328979, 575.362976,
               576.367004),
        intensity = c(324.171997, 29.08, 179.298996, 1954.572021, 194.485992,
                      1176.598999, 31.034, 316.217987, 19.666, 21.794001,
                      55.692001, 40.776001, 27.982, 42.688999, 48.007999,
                      145.162994, 34.383999, 0.624, 2.032, 1.357, 3.382, 1.338,
                      5.238, 4.136, 8.294, 3.376, 6.845, 0.766)
    )
    bpmz <- 489.659
    b <- cbind(
        mz = c(73.028, 87.043999, 89.059998, 99.07, 103.039001, 133.085999,
               143.106995, 147.065002, 177.112, 199.125, 221.138, 231.158997,
               235.117996, 283.175995, 293.196014, 327.201996, 337.223999,
               353.21701, 371.227997, 397.242004, 425.171997, 427.253998,
               454.276001, 455.248993, 472.286011, 529.312012, 547.328979,
               548.344971, 591.356995, 608.393982, 635.382996, 652.403015,
               679.411987, 696.434021, 723.435974, 740.45697, 767.45697,
               784.479004, 811.471985),
        intensity = c(121.129997, 394.515991, 1239.718994, 206.699997,
                      427.983002, 830.174988, 34.665001, 20.660999, 277.040985,
                      16.813, 85.750999, 15.945, 11.498, 7.878, 4.811, 6.001,
                      2.187, 3.93, 1.947, 2.531, 16.639, 3.748, 7.272, 2.639,
                      7.758, 0.797, 1.22, 0.7, 2.127, 0.797, 2.697, 1.089,
                      3.461, 2.321, 2.603, 2.696, 2.262, 2.222, 0.866)
    )
    dpmz <- 459.772
    d <- cbind(
        mz = c(73.028, 87.043999, 89.059998, 99.073997, 103.039001, 133.085999,
               143.106995, 177.112, 187.132996, 195.121994, 221.139008,
               231.158997, 239.149002, 283.174988, 293.196014, 306.065002,
               327.200989, 371.22699, 378.049011, 395.075012, 413.084991,
               415.253998, 442.277008, 503.307007, 513.320984, 531.333008,
               557.346008, 575.366028, 601.372009, 619.390991, 645.396973,
               663.41803, 664.414001, 707.440002, 708.440979, 751.46698),
        intensity = c(278.23999, 305.428009, 1729.343994, 254.378998,
                      123.156998, 1135.667969, 103.471001, 305.052002,
                      48.874001, 38.331001, 46.912998, 30.42, 33.466999,
                      25.868999, 19.299999, 15.968, 35.019001, 17.221001,
                      18.408001, 22.473, 13.028, 40.317001, 19.250999, 1.299,
                      1.099, 0.404, 0.361, 1.844, 0.476, 2.804, 0.359, 4.112,
                      0.398, 3.889, 0.447, 2.446)
    )
    epmz <- 432.28
    e <- cbind(
        mz = c(73.065002, 74.096001, 87.043999, 89.059998, 130.158997,
               131.070007, 133.085999, 175.097, 177.112, 195.123001, 221.138,
               239.149002, 265.165009, 283.174988, 309.190002, 327.201996,
               337.165009, 353.213013, 371.227997, 379.234009),
        intensity = c(18.827, 35.034, 110.706001, 1887.61499, 62.372002, 16.854,
                      1189.276978, 9.96, 385.527008, 3.877, 73.189003,
                      21.197001, 13.633, 28.17, 11.297, 42.102001, 2.14, 4.764,
                      38.998001, 1.858)
    )
    fpmz <- 344.229
    f <- cbind(
        mz = c(59.049, 73.042999, 74.096001, 87.043999, 89.059998, 107.07,
               130.158997, 133.085999, 177.112, 195.123001, 221.138, 239.149002,
               265.165009, 280.321014, 283.174988, 309.19101, 326.197998),
        intensity = c(0.702, 12.998, 14.456, 35.036999, 988.768005, 13.287,
                      6.312, 635.674988, 178.367004, 11.298, 28.547001, 29.573,
                      1.358, 1.233, 30.393999, 2.984, 2.935)
    )
    map <- join_gnps(a[, 1L], b[, 1L], apmz, bpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], b[map$y, ]), 2), 0.90)
    mapr <- join_gnps(b[, 1L], a[, 1L], bpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], b[map$y, ]), gnps(b[mapr$x, ], a[mapr$y, ]))
    map <- join_gnps(a[, 1L], d[, 1L], apmz, dpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], d[map$y, ]), 2), 0.90)
    mapr <- join_gnps(d[, 1L], a[, 1L], dpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], d[map$y, ]), gnps(d[mapr$x, ], a[mapr$y, ]))
    map <- join_gnps(a[, 1L], e[, 1L], apmz, epmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], e[map$y, ]), 2), 0.91)
    mapr <- join_gnps(e[, 1L], a[, 1L], epmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], e[map$y, ]), gnps(e[mapr$x, ], a[mapr$y, ]))
    map <- join_gnps(a[, 1L], f[, 1L], apmz, fpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], f[map$y, ]), 2), 0.91)
    mapr <- join_gnps(f[, 1L], a[, 1L], fpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], f[map$y, ]), gnps(f[mapr$x, ], a[mapr$y, ]))

    ## Third set
    apmz <- 488.358
    a <- cbind(
        mz = c(57.068001, 74.096001, 87.043999, 89.059998, 121.065002,
               133.085999, 147.080002, 165.091003, 177.112, 209.117004,
               221.139008, 233.190002, 253.143005, 271.153992, 277.216003,
               315.181, 321.243011, 359.207001, 365.21701, 383.179993,
               401.166992, 419.177002, 427.303986, 442.194, 471.330994),
        intensity = c(7114.173828, 1001.093994, 770.18103, 13194.55957,
                      6228.618164, 8398.287109, 1492.104004, 5111.643066,
                      2547.698975, 1840.756958, 245.207001, 3336.004883,
                      313.119995, 626.439026, 4527.278809, 461.109985,
                      682.629028, 8239.036133, 43.747002, 16.875999, 240.197998,
                      276.912994, 65.282997, 31.399, 14.15)
    )
    bpmz <- 356.28
    b <- cbind(
        mz = c(57.07, 74.096001, 89.059998, 121.065002, 133.085999, 139.074997,
               147.080002, 165.091003, 209.117996, 227.128006, 233.190002,
               250.901001, 268.912994, 277.216003, 303.231995, 309.901001,
               315.194, 321.239014),
        intensity = c(7689.937012, 1007.257996, 4579.356934, 8001.039062,
                      2327.36792, 881.854004, 446.681, 6326.22998, 497.213013,
                      17089.107422, 153.132996, 17.469999, 53.928001, 93.917,
                      5.337, 11.661, 3.787, 73.805)
    )
    map <- join_gnps(a[, 1L], b[, 1L], apmz, bpmz, tolerance = 0.1, ppm = 0)
    expect_equal(round(gnps(a[map$x, ], b[map$y, ]), 2), 0.86)
    mapr <- join_gnps(b[, 1L], a[, 1L], bpmz, apmz, tolerance = 0.1, ppm = 0)
    expect_equal(gnps(a[map$x, ], b[map$y, ]), gnps(b[mapr$x, ], a[mapr$y, ]))
})
