
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multgee: GEE Solver for Correlated Nominal or Ordinal Multinomial Responses

[![Github
version](https://img.shields.io/badge/GitHub%20-1.6.3-orange.svg)](%22commits/master%22)
[![Travis-CI Build
Status](https://travis-ci.org/AnestisTouloumis/multgee.svg?branch=master)](https://travis-ci.org/AnestisTouloumis/multgee)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

[![CRAN
Version](https://www.r-pkg.org/badges/version/multgee?color=blue)](https://cran.r-project.org/package=multgee)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/multgee?color=blue)](https://cranlogs.r-pkg.org/badges/grand-total/multgee)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/multgee)](https://cran.r-project.org/package=multgee)

## Installation

You can install the release version of `multgee`:

``` r
install.packages("multgee")
```

The source code for the release version of `multgee` is available on
CRAN at:

  - <https://CRAN.R-project.org/package=multgee>

Or you can install the development version of `multgee`:

``` r
# install.packages('devtools')
devtools::install_github("AnestisTouloumis/multgee")
```

The source code for the development version of `multgee` is available on
github at:

  - <https://github.com/AnestisTouloumis/multgee>

To use `multgee`, you should load the package as follows:

``` r
library("multgee")
#> Loading required package: gnm
#> Loading required package: VGAM
#> Loading required package: stats4
#> Loading required package: splines
```

## Usage

This package provides a generalized estimating equations (GEE) solver
for fitting marginal regression models with correlated nominal or
ordinal multinomial responses based on a local odds ratios
parameterization for the association structure (see Touloumis, Agresti
and Kateri, 2013).

There are two core functions to fit GEE models for correlated
multinomial responses:

  - `ordLORgee` for an ordinal response scale. Options for the marginal
    model include cumulative link models or an adjacent categories logit
    model,
  - `nomLORgee` for a nominal response scale. Currently, the only option
    is a marginal baseline category logit model.

The main arguments in both functions are:

  - an optional data frame (`data`),
  - a model formula (`formula`),
  - a cluster identifier variable (`id`),
  - an optional vector that identifies the order of the observations
    within each cluster (`repeated`).

The association structure among the correlated multinomial responses is
expressed via marginalized local odds ratios (Touloumis et al., 2013).
The estimating procedure for the local odds ratios can be summarized as
follows: For each level pair of the `repeated` variable, the available
responses are aggregated across clusters to form a square marginalized
contingency table. Treating these tables as independent, an RC-G(1) type
model is fitted in order to estimate the marginalized local odds ratios.
The `LORstr` argument determines the form of the marginalized local odds
ratios structure. Since the general RC-G(1) model is closely related to
the family of association models, one can instead fit an association
model to each of the marginalized contingency tables by setting `LORem =
"2way"` in the core functions.

There are also two utility functions:

  - `waldts` for assessing the goodness-of-fit of two nested GEE models
    based on a Wald test statistic,
  - `intrinsic.pars` for assessing whether the underlying association
    structure does not change dramatically across the level pairs of
    `repeated`.

## Example

The following R code replicates the GEE analysis presented in Touloumis
et al. (2013).

``` r
data("arthritis")
intrinsic.pars(y, arthritis, id, time, rscale = "ordinal")
#> [1] 0.6517843 0.9097341 0.9022272
```

The intrinsic parameters do not differ much. This suggests that the
uniform local odds ratios structure might be a good approximation for
the association
pattern.

``` r
fitord <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline), 
    data = arthritis, id = id, repeated = time)
summary(fitord)
#> GEE FOR ORDINAL MULTINOMIAL RESPONSES 
#> version 1.6.0 modified 2017-07-10 
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
#> beta10            -1.84003  0.38735 -4.7504      < 2e-16 ***
#> beta20             0.27712  0.34841  0.7954      0.42639    
#> beta30             2.24779  0.36509  6.1568      < 2e-16 ***
#> beta40             4.54824  0.41994 10.8307      < 2e-16 ***
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

You can obtain both the naive or model-based variance-covariance matrix

``` r
vcov(fitord, robust = FALSE)
#>                        beta10       beta20       beta30       beta40
#> beta10             0.13304581  0.102969547  0.101443437  0.102192784
#> beta20             0.10296955  0.108184084  0.107757454  0.108959319
#> beta30             0.10144344  0.107757454  0.118431905  0.120367843
#> beta40             0.10219278  0.108959319  0.120367843  0.144158270
#> factor(time)3     -0.00683553 -0.006841270 -0.006844379 -0.006845023
#> factor(time)5     -0.00485271 -0.005911092 -0.007096862 -0.008177379
#> factor(trt)2      -0.01418342 -0.015164249 -0.017047419 -0.018680253
#> factor(baseline)2 -0.09198334 -0.095927203 -0.098857314 -0.099278711
#> factor(baseline)3 -0.09116879 -0.096357499 -0.102006660 -0.103378186
#> factor(baseline)4 -0.09068621 -0.096505635 -0.105983764 -0.115011843
#> factor(baseline)5 -0.09047641 -0.096375221 -0.106442828 -0.126340274
#>                   factor(time)3 factor(time)5  factor(trt)2
#> beta10            -6.835530e-03 -0.0048527104 -1.418342e-02
#> beta20            -6.841270e-03 -0.0059110921 -1.516425e-02
#> beta30            -6.844379e-03 -0.0070968616 -1.704742e-02
#> beta40            -6.845023e-03 -0.0081773786 -1.868025e-02
#> factor(time)3      1.385890e-02  0.0068866925  8.931194e-05
#> factor(time)5      6.886693e-03  0.0119222345  3.656474e-04
#> factor(trt)2       8.931194e-05  0.0003656474  2.951549e-02
#> factor(baseline)2 -1.084973e-04  0.0003726858  5.702734e-04
#> factor(baseline)3 -6.122343e-05  0.0007162103  1.754002e-03
#> factor(baseline)4 -1.249254e-04  0.0014715004  2.688066e-03
#> factor(baseline)5 -1.271116e-04  0.0019351025  2.240042e-03
#>                   factor(baseline)2 factor(baseline)3 factor(baseline)4
#> beta10                -0.0919833355     -9.116879e-02     -0.0906862076
#> beta20                -0.0959272026     -9.635750e-02     -0.0965056353
#> beta30                -0.0988573138     -1.020067e-01     -0.1059837638
#> beta40                -0.0992787107     -1.033782e-01     -0.1150118435
#> factor(time)3         -0.0001084973     -6.122343e-05     -0.0001249254
#> factor(time)5          0.0003726858      7.162103e-04      0.0014715004
#> factor(trt)2           0.0005702734      1.754002e-03      0.0026880656
#> factor(baseline)2      0.1269602592      9.752685e-02      0.0986282227
#> factor(baseline)3      0.0975268537      1.150204e-01      0.1013843991
#> factor(baseline)4      0.0986282227      1.013844e-01      0.1459733478
#> factor(baseline)5      0.0988803723      1.021305e-01      0.1121556163
#>                   factor(baseline)5
#> beta10                -0.0904764144
#> beta20                -0.0963752208
#> beta30                -0.1064428278
#> beta40                -0.1263402737
#> factor(time)3         -0.0001271116
#> factor(time)5          0.0019351025
#> factor(trt)2           0.0022400419
#> factor(baseline)2      0.0988803723
#> factor(baseline)3      0.1021304546
#> factor(baseline)4      0.1121556163
#> factor(baseline)5      0.3073665070
```

or the robust or sandwich variance covariance matrix

``` r
vcov(fitord)
#>                         beta10       beta20       beta30       beta40
#> beta10             0.150036222  0.122662138  0.122932422  0.130479866
#> beta20             0.122662138  0.121391956  0.121481726  0.128900529
#> beta30             0.122932422  0.121481726  0.133289701  0.142022390
#> beta40             0.130479866  0.128900529  0.142022390  0.176350465
#> factor(time)3     -0.008019927 -0.006809123 -0.006792142 -0.006639313
#> factor(time)5     -0.005673879 -0.005072747 -0.006693716 -0.006966212
#> factor(trt)2      -0.020621470 -0.018408847 -0.019972498 -0.018404132
#> factor(baseline)2 -0.111673420 -0.107972858 -0.110550622 -0.116883295
#> factor(baseline)3 -0.110797179 -0.108378093 -0.113466831 -0.126935254
#> factor(baseline)4 -0.110639863 -0.112492005 -0.124620039 -0.142955578
#> factor(baseline)5 -0.113587658 -0.113575806 -0.124880707 -0.149744332
#>                   factor(time)3 factor(time)5  factor(trt)2
#> beta10            -0.0080199273 -0.0056738788 -0.0206214704
#> beta20            -0.0068091228 -0.0050727468 -0.0184088466
#> beta30            -0.0067921418 -0.0066937158 -0.0199724980
#> beta40            -0.0066393134 -0.0069662124 -0.0184041319
#> factor(time)3      0.0148312977  0.0075404098  0.0017285227
#> factor(time)5      0.0075404098  0.0130265123  0.0007814027
#> factor(trt)2       0.0017285227  0.0007814027  0.0279714947
#> factor(baseline)2 -0.0012255099 -0.0004173179  0.0035521933
#> factor(baseline)3 -0.0007208818  0.0004097354  0.0030426250
#> factor(baseline)4  0.0014636556  0.0034487913  0.0031348642
#> factor(baseline)5 -0.0002273939 -0.0009949330  0.0011349422
#>                   factor(baseline)2 factor(baseline)3 factor(baseline)4
#> beta10                -0.1116734197     -0.1107971788      -0.110639863
#> beta20                -0.1079728577     -0.1083780925      -0.112492005
#> beta30                -0.1105506218     -0.1134668314      -0.124620039
#> beta40                -0.1168832953     -0.1269352539      -0.142955578
#> factor(time)3         -0.0012255099     -0.0007208818       0.001463656
#> factor(time)5         -0.0004173179      0.0004097354       0.003448791
#> factor(trt)2           0.0035521933      0.0030426250       0.003134864
#> factor(baseline)2      0.1433485190      0.1080513149       0.112100685
#> factor(baseline)3      0.1080513149      0.1229627282       0.117793479
#> factor(baseline)4      0.1121006851      0.1177934794       0.168420444
#> factor(baseline)5      0.1145242718      0.1232078417       0.134411255
#>                   factor(baseline)5
#> beta10                -0.1135876579
#> beta20                -0.1135758062
#> beta30                -0.1248807072
#> beta40                -0.1497443322
#> factor(time)3         -0.0002273939
#> factor(time)5         -0.0009949330
#> factor(trt)2           0.0011349422
#> factor(baseline)2      0.1145242718
#> factor(baseline)3      0.1232078417
#> factor(baseline)4      0.1344112547
#> factor(baseline)5      0.2835126768
```

Finally, you can get the Wald confidence intervals for the regression
parameters, using either the naive or model-based variance-covariance
matrix

``` r
confint(fitord, robust = FALSE)
#>                        2.5 %       97.5 %
#> beta10            -2.5549366 -1.125125388
#> beta20            -0.3675379  0.921779261
#> beta30             1.5732894  2.922290954
#> beta40             3.8040767  5.292401998
#> factor(time)3     -0.2315206  0.229948277
#> factor(time)5     -0.5745073 -0.146494327
#> factor(trt)2      -0.8413559 -0.167909491
#> factor(baseline)2 -1.4012699 -0.004541444
#> factor(baseline)3 -1.9402922 -0.610861825
#> factor(baseline)4 -3.4046212 -1.906955516
#> factor(baseline)5 -5.0821694 -2.908936095
```

or the robust or sandwich variance covariance matrix

``` r
confint(fitord)
#>                        2.5 %      97.5 %
#> beta10            -2.5992134 -1.08084855
#> beta20            -0.4057572  0.95999854
#> beta30             1.5322296  2.96335073
#> beta40             3.7251701  5.37130863
#> factor(time)3     -0.2394781  0.23790571
#> factor(time)5     -0.5841988 -0.13680277
#> factor(trt)2      -0.8324304 -0.17683500
#> factor(baseline)2 -1.4449754  0.03916401
#> factor(baseline)3 -1.9628588 -0.58829522
#> factor(baseline)4 -3.4601391 -1.85143755
#> factor(baseline)5 -5.0391534 -2.95195213
```

## Getting help

The statistical methods implemented in `multgee` are described in
Touloumis et al. (2013). A detailed description of the functionality of
`multgee` can be found in Touloumis (2015). Note that an updated version
of this paper also serves as a vignette:

``` r
browseVignettes("multgee")
```

## How to cite

``` 

To cite multgee in publications use:

  Anestis Touloumis (2015). R Package multgee: A Generalized
  Estimating Equations Solver for Multinomial Responses. Journal
  of Statistical Software, 64(8), 1-14. URL
  http://www.jstatsoft.org/v64/i08/.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {{R} Package {multgee}: A Generalized Estimating Equations Solver for Multinomial Responses},
    author = {Anestis Touloumis},
    journal = {Journal of Statistical Software},
    year = {2015},
    volume = {64},
    number = {8},
    pages = {1--14},
    url = {http://www.jstatsoft.org/v64/i08/},
  }

To cite the methodology implemented in multgee in publications
use:

  Anestis Touloumis, Alan Agresti and Maria Kateri (2013). R
  Package multgee: A Generalized Estimating Equations Solver for
  Multinomial Responses. Biometrics, 69(3), 633-640. URL
  http://onlinelibrary.wiley.com/enhanced/doi/10.1111/biom.12054/.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {GEE for multinomial responses using a local odds ratios parameterization},
    author = {Anestis Touloumis and Alan Agresti and Maria Kateri},
    journal = {Biometrics},
    year = {2013},
    volume = {69},
    number = {3},
    pages = {633--640},
    url = {http://onlinelibrary.wiley.com/enhanced/doi/10.1111/biom.12054/},
  }
```

# References

<div id="refs" class="references">

<div id="ref-Touloumis2015">

Touloumis, A. (2015) R Package multgee: A Generalized Estimating
Equations Solver for Multinomial Responses. *Journal of Statistical
Software*, **64**, 1–14.

</div>

<div id="ref-Touloumis2013">

Touloumis, A., Agresti, A. and Kateri, M. (2013) GEE for Multinomial
Responses Using a Local Odds Ratios Parameterization. *Biometrics*,
**69**, 633–640.

</div>

</div>
