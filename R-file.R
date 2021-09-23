library(tidyverse)
library(dplyr)
DS <- read.csv("../census/inputs/census_income_original_2.csv")
###################################(bases)

DS %>% group_by(ID) %>% summarise(as.numeric(income_c)) -> Sum_income
colnames(Sum_income)[2] <- c("income_c")
Sum_income %>% summarise(sum(income_c)) -> Sum_income
DS %>% select(ID,age,income) -> D3
D1 <- left_join(Sum_income,D3)
D1 %>% distinct() %>% drop_na() -> D1
D1 %>% filter(age <25) -> under_25
under_25 <- mean(under_25$`sum(income_c)`)
D1 %>% filter(age >=25 & age <=50) -> till_50
till_50 <- mean(till_50$`sum(income_c)`)
D1 %>% filter(age> 50 & age<75) -> above_50
above_50 <- mean(above_50$`sum(income_c)`)
D1 %>% filter(age >75) -> above_75
above_75 <- mean(above_75$`sum(income_c)`)

#################################