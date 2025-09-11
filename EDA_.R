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

## Basic graph
data |> 
  ggplot(aes(x = Dose, y = VO2, color = as.factor(natural))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(.~Substrate)

## Facet by pair
for (i in unique(data$pair)){
  p <- data |> 
    filter(pair == i) |> 
    ggplot(aes(x = Dose, y = VO2, color = as.factor(natural))) +
    geom_point() +
    geom_smooth(method = "lm") + 
    facet_wrap(.~Substrate) +
    scale_color_manual(values = c("red","blue"),labels = c("0" = "Natural","1" = "Transgenic")) +
    labs(title = paste0("VO2 vs. Dose for pair ",i),
         color = "Genotype")
  print(p)
}



## Comparing substrate indicators vs. individual effects for each
amino <- lm(VO2 ~ Dose + natural + glutamate + pyruvate + palmytol + octanoyl, data = data)
substrate <- lm(VO2 ~ natural + Dose + Substrate, data = data)

AIC(amino, substrate)
summary(amino)$adj.r.squared
summary(substrate)$adj.r.squared

model_comp <- data.frame(
  Model = c("Amino Acids", "Substrate"),
  df    = c(8, 9),
  AIC   = c(7439.921, 7386.290),
  Adjusted_R2 = c(0.721, 0.755)
)

kable(
  model_comp,
  booktabs = TRUE,
  caption = "Model comparison using AIC and Adjusted R²"
) |>
  kable_styling(full_width = FALSE, position = "center")