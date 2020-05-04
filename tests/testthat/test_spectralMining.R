test_that("spectralMining: search for neutral loss", {
  
  # test spectrum from C. elegans sphingolipids (Cer(d17:0/22:0(OH)))
  x <- matrix(c(c(95.08628, 97.10201, 123.11601, 240.26794, 252.26850,
                  270.27852, 590.58606, 608.59827, 609.60059, 626.60829,
                  627.61158, 628.61115),
                c(344, 454, 374, 268, 1206,
                  996, 256, 2032, 1026, 7270,
                  2386, 298)),  ncol = 2,)
  
  colnames(x) <- c("mz", "intensity")
  
  # check for water NL
  result <- containsNeutralLoss(x,
                                 precursorMz = 626.60816, neutralLoss = 18.010565,
                                 tolerance = 0.005)

  ## Check decomposition
  expect_identical(result, TRUE)
  
  # test spectrum from C. elegans sphingolipids (Cer(d17:1/22:0(OH)))
  x <- matrix(c(c(238.25334, 250.25265, 251.25765, 268.26535, 606.58326,
                  607.58785, 624.59306, 625.59769, 626.60180),
                c(1088, 3852, 212, 200, 2178,
                  724, 1266, 584, 222)),  ncol = 2,)
  
  colnames(x) <- c("mz", "intensity")
  
  # check for water NL
  result <- containsNeutralLoss(x,
                                precursorMz = 624.59138, neutralLoss = 18.010565,
                                tolerance = 0.005)

  expect_identical(result, TRUE)

})

test_that("spectralMining: search for product ion", {
  
  # test spectrum from C. elegans sphingolipids (Cer(d17:0/22:0(OH)))
  x <- matrix(c(c(95.08628, 97.10201, 123.11601, 240.26794, 252.26850,
                  270.27852, 590.58606, 608.59827, 609.60059, 626.60829,
                  627.61158, 628.61115),
                c(344, 454, 374, 268, 1206,
                  996, 256, 2032, 1026, 7270,
                  2386, 298)),  ncol = 2,)
  
  colnames(x) <- c("mz", "intensity")
  
  # check for product ions
  result <- containsProductIon(x,
                               productMz = c(250.2529, 268.2635, 238.2530),
                               tolerance = 0.005)
  
  ## Check decomposition
  expect_identical(result, FALSE)
  
  # test spectrum from C. elegans sphingolipids (Cer(d17:1/22:0(OH)))
  x <- matrix(c(c(238.25334, 250.25265, 251.25765, 268.26535, 606.58326,
                  607.58785, 624.59306, 625.59769, 626.60180),
                c(1088, 3852, 212, 200, 2178,
                  724, 1266, 584, 222)),  ncol = 2,)
  
  colnames(x) <- c("mz", "intensity")
  
  # check for product ions
  result <- containsProductIon(x,
                               productMz = c(250.2529, 268.2635, 238.2530),
                               tolerance = 0.005)
  
  expect_identical(result, TRUE)

})