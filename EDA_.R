library(tidyverse)
library(readxl)
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
         natural = if_else(str_detect(Subject,"NT"),1,0),
         transgenic = if_else(str_detect(Subject,"Tg"),1,0),
         malate = if_else(str_detect(Substrate,"M"),1,0),
         glutamate = if_else(str_detect(Substrate,"G"),1,0),
         pyruvate = if_else(str_detect(Substrate,"P"),1,0),
         palmytol = if_else(str_detect(Substrate,"Pc"),1,0),
         octanoyl = if_else(str_detect(Substrate,"Oc"),1,0))

## Basic graph
data |> 
  ggplot(aes(x = Dose, y = VO2, color = as.factor(natural))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(.~Substrate)

summary(lm(VO2 ~ Dose + natural + glutamate + pyruvate + palmytol + octanoyl, data = data))
