# Core Utils for Mass Spectrometry Data

## Introduction

The `MsCoreUtils` package low-level functions for mass spectrometry data
and is independent of any high-level data structures
\[@rainer_modular_2022\]. These functions include mass spectra
processing functions (noise estimation, smoothing, binning),
quantitative aggregation functions (median polish, robust summarisation,
…), missing data imputation, data normalisation (quantiles, vsn, …) as
well as misc helper functions, that are used across high level data
structure within the R for Mass Spectrometry packages.

For a full list of function, see

``` r

library("MsCoreUtils")
ls(pos = "package:MsCoreUtils")
```

    ##  [1] "%between%"                  "aggregate_by_matrix"       
    ##  [3] "aggregate_by_vector"        "asInteger"                 
    ##  [5] "between"                    "bin"                       
    ##  [7] "breaks_ppm"                 "closest"                   
    ##  [9] "coefMA"                     "coefSG"                    
    ## [11] "coefWMA"                    "colCounts"                 
    ## [13] "colMeansMat"                "colSumsMat"                
    ## [15] "common"                     "common_path"               
    ## [17] "entropy"                    "estimateBaseline"          
    ## [19] "estimateBaselineConvexHull" "estimateBaselineMedian"    
    ## [21] "estimateBaselineSnip"       "estimateBaselineTopHat"    
    ## [23] "force_sorted"               "formatRt"                  
    ## [25] "getImputeMargin"            "gnps"                      
    ## [27] "group"                      "i2index"                   
    ## [29] "impute_bpca"                "impute_fun"                
    ## [31] "impute_knn"                 "impute_matrix"             
    ## [33] "impute_min"                 "impute_MinDet"             
    ## [35] "impute_MinProb"             "impute_mixed"              
    ## [37] "impute_mle"                 "impute_neighbour_average"  
    ## [39] "impute_QRILC"               "impute_RF"                 
    ## [41] "impute_with"                "impute_zero"               
    ## [43] "imputeMethods"              "isPeaksMatrix"             
    ## [45] "join"                       "join_gnps"                 
    ## [47] "localMaxima"                "maxi"                      
    ## [49] "medianPolish"               "navdist"                   
    ## [51] "ndotproduct"                "nentropy"                  
    ## [53] "neuclidean"                 "noise"                     
    ## [55] "normalize_matrix"           "normalizeMethods"          
    ## [57] "nspectraangle"              "ppm"                       
    ## [59] "rbindFill"                  "reduce"                    
    ## [61] "refineCentroids"            "rla"                       
    ## [63] "robustSummary"              "rowRla"                    
    ## [65] "rt2character"               "rt2numeric"                
    ## [67] "smooth"                     "sumi"                      
    ## [69] "validPeaksMatrix"           "valleys"                   
    ## [71] "vapply1c"                   "vapply1d"                  
    ## [73] "vapply1l"                   "which.first"               
    ## [75] "which.last"

or the [reference
page](https://rformassspectrometry.github.io/MsCoreUtils/reference/index.html)
on the package webpage.

## Examples

The functions defined in this package utilise basic classes with the aim
of being reused in packages that provide a more formal, high-level
interface.

As an examples, let’s take the
[`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)
function, that calculates the robust summary of the columns of a matrix:

``` r

x <- matrix(rnorm(30), nrow = 3)
colnames(x) <- letters[1:10]
rownames(x) <- LETTERS[1:3]
x
```

    ##            a            b          c          d          e           f
    ## A -1.4000435 -0.005571287 -1.8218177 -0.2827054  2.0650249 -1.86301149
    ## B  0.2553171  0.621552721 -0.2473253 -0.5536994 -1.6309894 -0.52201251
    ## C -2.4372636  1.148411606 -0.2441996  0.6289820  0.5124269 -0.05260191
    ##            g          h          i           j
    ## A  0.5429963  0.3629513  1.8885049 -0.01595031
    ## B -0.9140748 -1.3045435 -0.0974451 -0.82678895
    ## C  0.4681544  0.7377763 -0.9358474 -1.51239965

``` r

robustSummary(x)
```

    ##           a           b           c           d           e           f 
    ## -1.42529487  0.58813101 -0.77111419 -0.06914093  0.41024509 -0.81254197 
    ##           g           h           i           j 
    ##  0.03235865 -0.06793866  0.19504940 -0.78504631

This function is typicall to be used to summarise peptide quantitation
values into protein intensities[^1]. This functionality is available in

- the
  [MSnbase::combineFeatures()](http://lgatto.github.io/MSnbase/reference/combineFeatures.md)
  function for `MSnSet` objects and

- the
  [QFeatures::aggregateFeatures()](https://rformassspectrometry.github.io/QFeatures/reference/Features-aggregate.html)
  function for `QFeatures` objects.

## Contributions

If you would like to contribute any low-level functionality, please
[open a GitHub
issue](https://github.com/RforMassSpectrometry/MsCoreUtils/issues) to
discuss it. Please note that any
[contributions](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions)
should follow the [style
guide](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#coding-style)
and will require an appropriate unit test.

If you wish to reuse any functions in this package, please just go
ahead. If you would like any advice or seek help, please either [open a
GitHub
issue](https://github.com/RforMassSpectrometry/MsCoreUtils/issues).

## Session information

    ## R Under development (unstable) (2025-11-22 r89050)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] MsCoreUtils_1.23.1 BiocStyle_2.39.0  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.5           knitr_1.50          rlang_1.1.6        
    ##  [4] xfun_0.54           generics_0.1.4      textshaping_1.0.4  
    ##  [7] clue_0.3-66         jsonlite_2.0.0      S4Vectors_0.49.0   
    ## [10] htmltools_0.5.8.1   stats4_4.6.0        ragg_1.5.0         
    ## [13] sass_0.4.10         rmarkdown_2.30      evaluate_1.0.5     
    ## [16] jquerylib_0.1.4     MASS_7.3-65         fastmap_1.2.0      
    ## [19] yaml_2.3.10         lifecycle_1.0.4     bookdown_0.45      
    ## [22] BiocManager_1.30.27 cluster_2.1.8.1     compiler_4.6.0     
    ## [25] fs_1.6.6            htmlwidgets_1.6.4   systemfonts_1.3.1  
    ## [28] digest_0.6.39       R6_2.6.1            bslib_0.9.0        
    ## [31] tools_4.6.0         BiocGenerics_0.57.0 pkgdown_2.2.0      
    ## [34] cachem_1.1.0        desc_1.4.3

## References

[^1]: See Sticker *et al.* Robust summarization and inference in
    proteome-wide label-free quantification.
    <https://doi.org/10.1101/668863>.
