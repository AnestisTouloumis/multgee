<!-- README.md is generated from README.Rmd. Please edit that file -->
multgee: GEE Solver for Correlated Nominal or Ordinal Multinomial Responses
===========================================================================

[![Travis-CI Build Status](https://travis-ci.org/AnestisTouloumis/multgee.svg?branch=master)](https://travis-ci.org/AnestisTouloumis/multgee) [![develVersion](https://img.shields.io/badge/devel%20version-1.5.5-brightgreen.svg?style=flat)](https://github.com/AnestisTouloumis/multgee) [![Project Status: Active The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--06--15-brightgreen.svg)](/commits/master)

[![CRAN Version](http://www.r-pkg.org/badges/version/multgee?color=blue)](https://cran.r-project.org/package=multgee) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/multgee?color=blue)](http://cranlogs.r-pkg.org/badges/grand-total/multgee) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/multgee)](http://cran.rstudio.com/web/packages/multgee/index.html)

Author
------

Anestis Touloumis <https://brighton.academia.edu/AnestisTouloumis>

School of Computing, Engineering and Mathematics, University of Brighton.

Installation
------------

You can install the release version of the **multgee** package from CRAN:

``` r
install.packages("multgee")
```

or the development version from github:

``` r
install.packages("devtools")  # if you have not installed 'devtools' package
devtools::install_github("AnestisTouloumis/multgee")
```

The source code for the **multgee** package is available on github at

-   <https://github.com/AnestisTouloumis/multgee>.

To use **multgee**, you should load the package as follows:

``` r
library(multgee)
#> Loading required package: gnm
#> Loading required package: VGAM
#> Loading required package: stats4
#> Loading required package: splines
```

Usage and functions
-------------------

The **multgee** package provides a generalized estimating equations (GEE) solver for fitting marginal regression models with correlated nominal or ordinal multinomial responses based on a local odds ratios parameterization for the association structure (Touloumis 2015).

The package contains two core functions that fit GEE models for correlated multinomial responses:

-   `ordLORgee` for an ordinal response scale. Options for the marginal model include cumulative link models or an adjacent categories logit model,
-   `nomLORgee` for a nominal response scale. A marginal baseline category logit model can be fitted.

The main arguments in both functions are:

-   an optional data frame `data`,
-   a model formula `formula`,
-   a cluster identifier variable `id`,
-   an optional vector that identifies the order of the observations within each cluster `repeated`.

The association structure among the correlated multinomial responses is expressed via marginalized local odds ratios (Touloumis, Agresti, and Kateri 2013). The estimating procedure for the local odds ratios can be summarized as follows: For each level pair of the `repeated` variable, the available responses are aggregated across clusters to form a square marginalized contingency table. Treating these tables as independent, an RC-G(1) type model is fitted in order to estimate the marginalized local odds ratios. The `LORstr` argument determines the form of the marginalized local odds ratios structure. Since the general RC-G(1) model is closely related to the family of association models, one can instead fit anassociation model to each of the marginalized contingency tables by setting `LORem="2way"` in the core functions.

The **multgee** package offers two utility functions:

-   `waldts` for assessing the goodness-of-fit of two nested GEE models based on a Wald test statistic,
-   `intrinsic.pars` for assessing whether the underlying association structure does not change dramatically across the level pairs of `repeated`.

Example
-------

The following R code illustrates how to use the core functions of **multgee**.

``` r
data(arthritis)
fitord <- ordLORgee(y ~ factor(time) + factor(trt) + factor(baseline), data = arthritis, 
    id = id, repeated = time)
summary(fitord)
#> GEE FOR ORDINAL MULTINOMIAL RESPONSES 
#> version 1.5.1 modified 2015-03-09 
#> 
#> Link : Cumulative logit 
#> 
#> Local Odds Ratios:
#> Structure:         category.exch
#> Model:             3way
#> 
#> call:
#> ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline), 
#>     data = arthritis, id = id, repeated = time)
#> 
#> Summary of residuals:
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -0.5176739 -0.2380475 -0.0737290 -0.0001443 -0.0066846  0.9933154 
#> 
#> Number of Iterations: 6 
#> 
#> Coefficients:
#>                   Estimate   san.se   san.z Pr(>|san.z|)    
#> beta01            -1.84003  0.38735 -4.7504      < 2e-16 ***
#> beta02             0.27712  0.34841  0.7954      0.42639    
#> beta03             2.24779  0.36509  6.1568      < 2e-16 ***
#> beta04             4.54824  0.41994 10.8307      < 2e-16 ***
#> factor(time)3     -0.00079  0.12178 -0.0065      0.99485    
#> factor(time)5     -0.36050  0.11413 -3.1586      0.00159 ** 
#> factor(trt)2      -0.50463  0.16725 -3.0173      0.00255 ** 
#> factor(baseline)2 -0.70291  0.37861 -1.8565      0.06338 .  
#> factor(baseline)3 -1.27558  0.35066 -3.6376      0.00028 ***
#> factor(baseline)4 -2.65579  0.41039 -6.4714      < 2e-16 ***
#> factor(baseline)5 -3.99555  0.53246 -7.5040      < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Local Odds Ratios Estimates:
#>        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
#>  [1,] 0.000 0.000 0.000 0.000 1.919 1.919 1.919 1.919 2.484 2.484 2.484
#>  [2,] 0.000 0.000 0.000 0.000 1.919 1.919 1.919 1.919 2.484 2.484 2.484
#>  [3,] 0.000 0.000 0.000 0.000 1.919 1.919 1.919 1.919 2.484 2.484 2.484
#>  [4,] 0.000 0.000 0.000 0.000 1.919 1.919 1.919 1.919 2.484 2.484 2.484
#>  [5,] 1.919 1.919 1.919 1.919 0.000 0.000 0.000 0.000 2.465 2.465 2.465
#>  [6,] 1.919 1.919 1.919 1.919 0.000 0.000 0.000 0.000 2.465 2.465 2.465
#>  [7,] 1.919 1.919 1.919 1.919 0.000 0.000 0.000 0.000 2.465 2.465 2.465
#>  [8,] 1.919 1.919 1.919 1.919 0.000 0.000 0.000 0.000 2.465 2.465 2.465
#>  [9,] 2.484 2.484 2.484 2.484 2.465 2.465 2.465 2.465 0.000 0.000 0.000
#> [10,] 2.484 2.484 2.484 2.484 2.465 2.465 2.465 2.465 0.000 0.000 0.000
#> [11,] 2.484 2.484 2.484 2.484 2.465 2.465 2.465 2.465 0.000 0.000 0.000
#> [12,] 2.484 2.484 2.484 2.484 2.465 2.465 2.465 2.465 0.000 0.000 0.000
#>       [,12]
#>  [1,] 2.484
#>  [2,] 2.484
#>  [3,] 2.484
#>  [4,] 2.484
#>  [5,] 2.465
#>  [6,] 2.465
#>  [7,] 2.465
#>  [8,] 2.465
#>  [9,] 0.000
#> [10,] 0.000
#> [11,] 0.000
#> [12,] 0.000
#> 
#> pvalue of Null model: <0.0001
```

Getting help
------------

Details about the methods implemented in *multgee* can be found in Touloumis, Agresti, and Kateri (2013). A detailed description of *multgee* can be found in Touloumis (2015). An updated version of this paper also serves as vignette for *multgee* which can be accessed by executing the following R commands:

``` r
browseVignettes("multgee")
```

How to cite
-----------

``` r
citation("multgee")
#> 
#> To cite multgee in publications use:
#> 
#>   Anestis Touloumis (2015). R Package multgee: A Generalized
#>   Estimating Equations Solver for Multinomial Responses. Journal
#>   of Statistical Software, 64(8), 1-14. URL
#>   http://www.jstatsoft.org/v64/i08/.
#> 
#> To cite the methodology implemented in multgee in publications
#> use:
#> 
#>   Anestis Touloumis, Alan Agresti and Maria Kateri (2013). R
#>   Package multgee: A Generalized Estimating Equations Solver for
#>   Multinomial Responses. Biometrics, 69(3), 633-640. URL
#>   http://onlinelibrary.wiley.com/enhanced/doi/10.1111/biom.12054/.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

References
==========

Touloumis, Anestis. 2015. “R Package Multgee: A Generalized Estimating Equations Solver for Multinomial Responses.” *Journal of Statistical Software* 64 (3): 1–14. doi:[10.18637/jss.v064.i08](https://doi.org/10.18637/jss.v064.i08).

Touloumis, Anestis, Alan Agresti, and Maria Kateri. 2013. “GEE for Multinomial Responses Using a Local Odds Ratios Parameterization.” *Biometrics* 69 (3): 633–40. doi:[10.1111/biom.12054](https://doi.org/10.1111/biom.12054).
