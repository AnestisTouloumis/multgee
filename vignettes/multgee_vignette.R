## -----------------------------------------------------------------------------
library("multgee")
data("arthritis")
head(arthritis)
intrinsic.pars(y = y, data = arthritis, id = id, repeated = time,
                  rscale = "ordinal")

## -----------------------------------------------------------------------------
 fit <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
        link = "logit", id = id, repeated = time, data = arthritis,
        LORstr = "uniform")
 summary(fit)

## -----------------------------------------------------------------------------
fit1 <- update(fit, formula = ~. + factor(sex) + age)
waldts(fit, fit1)

## -----------------------------------------------------------------------------
confint(fit)

