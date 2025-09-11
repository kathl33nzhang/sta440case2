library(lme4)
library(performance)
library(lmtest)
library(nlme)

## Different parameterizations but same model
a <- model.matrix(VO2 ~ Substrate : natural + Dose  : natural : Substrate, data = data)
b <- model.matrix(VO2 ~ natural * Dose * Substrate, data = data)
summary(lm(VO2 ~ Substrate : natural + Dose  : natural : Substrate, data = data))
summary(lm(VO2 ~ natural * Dose * Substrate, data = data))

## 1-way ANOVA on substrate, estimate for group mean
## Can calculate our own p-values comparing to overall mean?
summary(lm(VO2 ~ Substrate - 1, data = data))

## Random effects model with pair as a random intercept
lmm1 <- lmer(VO2 ~ natural * Dose * Substrate + (1 | pair), data = data)
summary(lmm1)
performance::r2(lmm1)
performance::icc(lmm1)
ICC = 423309/(423309+867144)

## Comparison to no group effect, we see a higher r2 for RE model, LRT shows higher LogLik for LMM
lm_compare <- lm(VO2 ~ natural * Dose * Substrate , data = data)
summary(lm_compare)
lrtest(lmm1,lm_compare)

## Comparisn to GLS model, not working rn but will look into

gls_compare <-nlme::gls(VO2 ~ natural * Dose * Substrate,
                        corr=corCompSymm(value=0.5,form=~1|pair),data=data)
summary(gls.out)

## Looking at residuals for LMM model
par(mfrow=c(1,2),las=1)
qqnorm(residuals(lmm1),main="Residuals")
qqnorm(unlist(ranef(lmm1)$pair),main="Random Effects")
