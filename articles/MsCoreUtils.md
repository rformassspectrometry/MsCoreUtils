# Core Utils for Mass Spectrometry Data

## Introduction

The *MsCoreUtils* package provides low-level functions for mass
spectrometry data and is independent of any high-level data structures
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
    ## [27] "gnps_chain_dp"              "gnps_r"                    
    ## [29] "group"                      "i2index"                   
    ## [31] "impute_bpca"                "impute_fun"                
    ## [33] "impute_knn"                 "impute_matrix"             
    ## [35] "impute_min"                 "impute_MinDet"             
    ## [37] "impute_MinProb"             "impute_mixed"              
    ## [39] "impute_mle"                 "impute_neighbour_average"  
    ## [41] "impute_QRILC"               "impute_RF"                 
    ## [43] "impute_with"                "impute_zero"               
    ## [45] "imputeMethods"              "isPeaksMatrix"             
    ## [47] "join"                       "join_gnps"                 
    ## [49] "join_gnps_r"                "localMaxima"               
    ## [51] "maxi"                       "medianPolish"              
    ## [53] "navdist"                    "ndotproduct"               
    ## [55] "nentropy"                   "neuclidean"                
    ## [57] "noise"                      "normalize_matrix"          
    ## [59] "normalizeMethods"           "nspectraangle"             
    ## [61] "ppm"                        "rbindFill"                 
    ## [63] "reduce"                     "refineCentroids"           
    ## [65] "retry"                      "rla"                       
    ## [67] "robustSummary"              "rowRla"                    
    ## [69] "rt2character"               "rt2numeric"                
    ## [71] "smooth"                     "sumi"                      
    ## [73] "validPeaksMatrix"           "valleys"                   
    ## [75] "vapply1c"                   "vapply1d"                  
    ## [77] "vapply1l"                   "which.first"               
    ## [79] "which.last"

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

    ## R Under development (unstable) (2026-04-05 r89793)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
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
    ## [1] MsCoreUtils_1.23.7 BiocStyle_2.39.0  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.5           knitr_1.51          rlang_1.2.0        
    ##  [4] xfun_0.57           otel_0.2.0          generics_0.1.4     
    ##  [7] textshaping_1.0.5   clue_0.3-68         jsonlite_2.0.0     
    ## [10] S4Vectors_0.49.1    htmltools_0.5.9     stats4_4.7.0       
    ## [13] ragg_1.5.2          sass_0.4.10         rmarkdown_2.31     
    ## [16] evaluate_1.0.5      jquerylib_0.1.4     MASS_7.3-65        
    ## [19] fastmap_1.2.0       yaml_2.3.12         lifecycle_1.0.5    
    ## [22] bookdown_0.46       BiocManager_1.30.27 cluster_2.1.8.2    
    ## [25] compiler_4.7.0      fs_2.0.1            htmlwidgets_1.6.4  
    ## [28] systemfonts_1.3.2   digest_0.6.39       R6_2.6.1           
    ## [31] bslib_0.10.0        tools_4.7.0         BiocGenerics_0.57.0
    ## [34] pkgdown_2.2.0       cachem_1.1.0        desc_1.4.3

## References

[^1]: See Sticker *et al.* Robust summarization and inference in
    proteome-wide label-free quantification.
    <https://doi.org/10.1101/668863>.
