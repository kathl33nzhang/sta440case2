library(lme4)
library(performance)
library(lmtest)
library(nlme)
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
data = NULL
for (i in excel_sheets("data.xls")){
  this_df <- read_excel("data.xls",sheet = i,skip = 1) |> 
    select(-Basal) |> 
    pivot_longer(cols = -Subject, names_to = "Dose",values_to = "VO2") |> 
    mutate(Substrate = i)
  data <- rbind(data,this_df)
}
data <- data |> 
  mutate(Dose = round(as.numeric(Dose), 2),
         natural = if_else(str_detect(Subject,"NT"),"Natural","Transgenic"),
         malate = if_else(str_detect(Substrate,"M"),1,0),
         glutamate = if_else(str_detect(Substrate,"G"),1,0),
         pyruvate = if_else(str_detect(Substrate,"P"),1,0),
         palmytol = if_else(str_detect(Substrate,"Pc"),1,0),
         octanoyl = if_else(str_detect(Substrate,"Oc"),1,0),
         pair = substr(Subject,3,3))

## Should we filter out the 12.95 measure? it isn't basal but fit is much better without it
## May also need to log transform outcome variable,
data <- data |> 
  filter(Dose < -13)

## Different parameterizations but same model
## Not important for final presentation but cool for referenceß
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
## calculating the same thing from summary output
ICC = 582983/(582983+322707)

## Comparison to no group effect, we see a higher r2 for RE model, LRT shows higher LogLik for LMM
lm_compare <- lm(VO2 ~ natural * Dose * Substrate , data = data)
summary(lm_compare)
lrtest(lmm1,lm_compare)

## Looking at residuals vs. theoretical quantiles for LMM model
par(mfrow=c(1,2),las=1)
qqnorm(residuals(lmm1),main="Residuals")
qqnorm(unlist(ranef(lmm1)$pair),main="Random Effects")

## Looking at residuals for other predictors
data |> 
  drop_na() |> 
  mutate(residuals = residuals(lmm1)) |> 
  ggplot(aes(x = natural,y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0 , color = "blue", linetype = 2, linewidth = 2)

## This one looks bad without dropping 12.95 observations
data |> 
  drop_na() |> 
  mutate(residuals = residuals(lmm1)) |> 
  ggplot(aes(x = Dose,y = residuals)) +
  geom_point()+
  geom_hline(yintercept = 0 , color = "blue", linetype = 2, linewidth = 2)

data |> 
  drop_na() |> 
  mutate(residuals = residuals(lmm1)) |> 
  ggplot(aes(x = Substrate,y = residuals)) +
  geom_point()+
  geom_hline(yintercept = 0 , color = "blue", linetype = 2, linewidth = 2)

data |> 
  drop_na() |> 
  mutate(residuals = residuals(lmm1)) |> 
  ggplot(aes(x = pair,y = residuals)) +
  geom_point()+
  geom_hline(yintercept = 0 , color = "blue", linetype = 2, linewidth = 2)

## Fitting model to data
## TODO: fix key, common scales on y-axis
for (i in unique(data$pair)){
  data_pair <- data |> 
    filter(pair == i)
p <- data_pair |> 
  mutate(lmmpredictions = predict(lmm1,data_pair),
         lmpredictions = predict(lm_compare,data_pair)) |> 
  ggplot(aes(x = Dose, shape = natural)) +
  geom_point(aes(y = VO2),color = "black") +
  geom_point(aes(y = lmmpredictions),color = "blue") +
  geom_smooth(aes(y = lmmpredictions),method = "lm") +
  #geom_point(aes(y = lmpredictions),color = "red") +
  facet_wrap(.~Substrate) +
  labs(title = paste0("VO2 vs. Dose for pair ",i),
       color = "Model")
print(p)
}

## Comparing to og linear regression
## TODO: fix key, common scales on y-axis
for (i in unique(data$pair)){
  data_pair <- data |> 
    filter(pair == i)
  p <- data_pair |> 
    mutate(lmmpredictions = predict(lmm1,data_pair),
           lmpredictions = predict(lm_compare,data_pair)) |> 
    ggplot(aes(x = Dose, shape = natural)) +
    #geom_point(aes(y = VO2),color = "black") +
    geom_point(aes(y = lmmpredictions),color = "blue") +
    geom_point(aes(y = lmpredictions),color = "red") +
    facet_wrap(.~Substrate) +
    labs(title = paste0("VO2 vs. Dose for pair ",i),
         color = "Model")
  print(p)
}
